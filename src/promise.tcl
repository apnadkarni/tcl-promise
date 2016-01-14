#
# Copyright (c) 2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file license.terms for license

namespace eval promise {}

proc promise::lambda {arguments body args} {
    return [list ::apply [list $arguments $body] {*}$args]
}

# Credits:
# This implementation is based on the spec and tutorials at
# https://promisesaplus.com/
# https://www.promisejs.org,
# https://github.com/domenic/promises-unwrapping,
# https://github.com/kriskowal/q/blob/v1/design/README.js,

catch {promise::Promise destroy}
oo::class create promise::Promise {

    # The promise state can be one of
    #  PENDING - Initial state where it has not yet been assigned a
    #            value or error
    #  FULFILLED - The promise has been assigned a value
    #  REJECTED  - The promise has been assigned an error
    #  ATTACHED  - The promise is attached to another promise
    variable _state

    # The promise value once it is fulfilled or rejected. In the latter
    # case, it should be an error dictionary by convention
    variable _value

    # Handlers to be notified when the promise is rejected. Each element
    # in this list is a pair consisting of the fulfilment handler
    # and the rejection handler. Either element of the pair could be
    # empty signifying no handler for that case. The list is populated
    # via the then method.
    variable _handlers

    # Reference counting to free up promises since Tcl does not have
    # garbage collection for objects. An *internal* reference count
    # is maintained when callbacks are scheduled so that a promise
    # is not released while a callback queued on the event loop holds
    # a reference. Additionally, garbage collection via reference
    # counting only takes place after at least one done/then callback
    # is placed on the event queue, not before. Else promises that
    # are immediately resolved on construction would be freed right
    # away before the application even gets a chance to call done/then.
    variable _nrefs
    
    constructor {cmd} {
        set _state PENDING
        set _handlers [list ]
        set _nrefs 0
        set _at_least_one_handler_run 0
        
        # Errors in the construction command are returned via
        # the standard mechanism of reject.
        if {[catch {
            uplevel #0 [linsert $cmd end [self]]
        } msg edict]} {
            my reject [list $msg $edict]
        }
    }

    destructor {
    }
    
    method state {} { return $_state }
    
    method value {} {
        # Note even if _value is set, it may be not the "final" value
        if {$_state ni {FULFILLED REJECTED}} {
            error "Value is not set."
        }
        return $_value
    }

    method ref {} {
        incr _nrefs
    }

    method unref {} {
        incr _nrefs -1
        my GC
    }
    
    method GC {} {
        if {$_nrefs <= 0 && [llength $_handlers] == 0} {
            my destroy
        }
    }
    
    method ResolveAttached {value} {
        if {$_state ne "ATTACHED"} {
            return
        }
        set _value $value
        set _state FULFILLED
        my ScheduleHandlers
        return
    }
    
    method RejectAttached {erval} {
        if {$_state ne "ATTACHED"} {
            return
        }
        set _value $erval
        set _state REJECTED
        my ScheduleHandlers
        return
    }
    
    # Method to invoke to fulfil a promise with a value or another promise.
    method resolve {value} {
        if {$_state ne "PENDING"} {
            return 0;             # Already settled
        }
        set _value $value
        set _state FULFILLED
        my ScheduleHandlers
        return 1
    }

    # Method to invoke to fulfil a promise with a value or another promise.
    method resolve_with_promise {promise} {
        if {$_state ne "PENDING"} {
            return 0;             # Already settled
        }
        if {[catch {
            $promise done [namespace code {my ResolveAttached}] [namespace code {my RejectAttached}]
        } msg edict]} {
            my reject [list $msg $edict]
        } else {
            set _state ATTACHED
        }
        
        return 1
    }

    # Method to invoke handlers when promise is rejected.
    # By convention, errval should be [list message error_dictionary]
    method reject {errval} {
        if {$_state ne "PENDING"} {
            return 0;             # Already settled
        }
        set _value $errval
        set _state REJECTED
        my ScheduleHandlers
        return 1
    }

    # Internal method to queue all registered handlers based on
    # whether the promise is succesfully fulfilled or not
    method ScheduleHandlers {} {
        if {$_state ni {FULFILLED REJECTED} || [llength $_handlers] == 0 } {
            # Promise is not settled or no handlers registered
            return
        }

        set _at_least_one_handler_run 1; # Needed for garbage collection
        if {$_state eq "FULFILLED"} {
            set ix 0
        } else {
            set ix 1
        }

        foreach pair $_handlers {
            set cmd [lindex $pair $ix]
            if {[llength $cmd]} {
                # Enqueue the callback via the event loop passing $_value
                after 0 [list after idle [linsert $cmd end $_value]]
            }
        }
        set _handlers [list ]
        my GC
        return 
    }        

    method done {on_success {on_error {}}} {
        # TBD - as per the Promise/A+ spec, errors in done should generate
        # a background error (unlike then).
        lappend _handlers [list $on_success $on_error]
        # In case promise already fulfilled, we will need to run the handlers
        my ScheduleHandlers
        return
    }
    
    method then {on_success {on_error {}}} {
        return [[self class] new [list apply [list {predecessor on_success on_error prom} {
            $predecessor done \
                 [list ::promise::_then_handler $prom FULFILLED $on_success] \
                 [list ::promise::_then_handler $prom REJECTED $on_error]
        }] [self] $on_success $on_error]]
    }
}

proc promise::_then_handler {target_promise status cmd value} {
    # Run the specified command and resolve/reject the target promise
    # accordingly. If the command is empty, the passed-in value is passed
    # on to the target promise.

    # IMPORTANT!!!!
    # MUST BE CALLED FROM EVENT LOOP AT so info level must be 1. Else
    # promise::resolve/reject will not work
    # Also, Do NOT change the param name target_promise without changing
    # those procs.
    # Oh what a hack to get around lack of closures. Alternative would have
    # been to pass an additional parameter (target_promise)
    # to the application code but then that script would have had to
    # carry that around.

    if {[info level] != 1} {
        error "Internal error: _then_handler not at level 1"
    }
    
    if {[llength $cmd] == 0} {
        switch -exact -- $status {
            FULFILLED { $target_promise resolve $value }
            REJECTED  { $target_promise reject $value }
            ATTACHED -
            PENDING  -
            default {
                $target_promise reject [promise::_make_errval PROMISE THEN STATE "Internal error: invalid status $state"]
            }
        }
    } else {
        # Invoke the real handler code and resolve/reject the target promise.
        # Not the handler code may have called one of the promise::then_*
        # commands itself in which case these calls will be no-ops.
        # TBD - ideally we would like to execute at global level. However
        # the then_* commands retrieve target_promise from level 1 (here).
        # So directly invoke.
        if {[catch [linsert $cmd end $value] value edict]} {
            $target_promise reject [list $value $edict]
        } else {
            $target_promise resolve $value
        }
    }
    return
}

proc promise::then_resolve {value} {
    # TBD - what if someone calls this from within a uplevel #0 ? The
    # upvar will be all wrong
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_resolve called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise resolve $value
}

proc promise::then_promise {promise} {
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_promise called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise resolve_with_promise $promise
}

proc promise::then_reject {errval} {
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_reject called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise reject $errval
}

proc promise::all {promises} {
    set all_promise [Promise new [lambda {promises prom} {
        if {[llength $promises] == 0} {
            $prom resolve {}
            return
        }

        set promises [lassign $promises first_promise]
        $first_promise done \
            [list ::promise::_all_helper $prom $promises {} FULFILLED] \
            [list ::promise::_all_helper $prom $promises {} REJECTED]
    } $promises]]
        
    return $all_promise
}
          
# Callback for promise::all.
#  all_promise - the "master" promise returned by the all call.
#  remaining_promises - the list of remaining promises still to be resolved.
#  values - values collected so far from resolved promises
#  resolution - whether the current promise was resolved with "FULFILLED"
#   or "REJECTED"
#  value - the value of the currently resolved promise or error description
#   in case rejected
proc promise::_all_helper {all_promise remaining_promises values resolution value} {
    if {![info object isa object $all_promise]} {
        # The object has been deleted. Naught to do
        return
    }
    if {$resolution eq "REJECTED"} {
        # This promise failed. Immediately reject the master promise
        # TBD - can we somehow indicate which promise failed ?
        $all_promise reject $value
        return
    }
    lappend values $value
    if {[llength $remaining_promises] == 0} {
        # No more promises left. All done.
        $all_promise resolve $values
    }

    # Wait on the remaining promises
    set remaining_promises [lassign $remaining_promises next]
    if {[catch {
        $next done \
            [list [namespace current]::_all_helper $all_promise $remaining_promises $values FULFILLED] \
            [list [namespace current]::_all_helper $all_promise $remaining_promises $values REJECTED]
    } msg edict]} {
        $all_promise reject [list $msg $edict]
    }
    return
}

proc promise::race {promises} {
    set race_promise [Promise new [lambda {promises prom} {
        if {[llength $promises] == 0} {
            $prom reject [_make_errval PROMISE RACE EMPTYSET "Promise set is empty"]
            return
        }
        foreach promise $promises {
            $promise done [list $prom resolve] [list $prom reject]
        }
    } $promises]]

    return $race_promise
}

proc promise::_make_errval {args} {
    catch {throw $args [lindex $args end]} msg edict
    return [list $msg $edict]
}
                            
package provide promise 0.1

package require http
proc checkurl {url} {
    set prom [promise::Promise new [promise::lambda {url prom} {
        http::geturl $url -method HEAD -command [promise::lambda {prom tok} {
            upvar #0 $tok http_state
            $prom resolve [list $http_state(url) $http_state(status)]
            ::http::cleanup $tok
        } $prom]
    } $url]]
    return $prom
}

proc checkurls {urls} {
    return [promise::all [lmap url $urls {checkurl $url}]]
}

proc promise::pgeturl {url args} {
    uplevel #0 {package require http}
    proc [namespace current]::pgeturl {url args} {
        set prom [promise::Promise new [lambda {http_args prom} {
            http::geturl {*}$http_args -command [promise::lambda {prom tok} {
                upvar #0 $tok http_state
                if {$http_state(status) eq "ok"} {
                    $prom resolve [array get http_state]
                } else {
                    $prom reject [array get http_state]
                }
                ::http::cleanup $tok
            } $prom]
        } [linsert $args 0 $url]]]
        return $prom
    }
    tailcall pgeturl $url {*}$args
}

proc promise::ptimer {delay {value "Timer expired."}} {
    return [promise::Promise new [lambda {delay prom} {
        after $delay [list $prom resolve $value]
    } $delay]]
}

proc promise::ptimeout {delay {value "Operation timed out."}} {
    return [promise::Promise new [lambda {delay prom} {
        after $delay [list $prom reject $value]
    } $delay]]
}

proc promise::pconnect {args} {
    return [Promise new [lambda {so_args prom} {
        set so [socket -async {*}$so_args]
        fileevent $so writable [promise::lambda {prom so args} {
            fileevent $so writable {}
            $prom resolve [linsert $args 0 $so]
        } $prom $so]
    } $args]]
}

proc promise::ReadChannel {prom chan data} {
    set newdata [read $chan]
    if {[string length $newdata] || ![eof $chan]} {
        append data $newdata
        fileevent $chan readable [list [namespace current]::ReadChannel $prom $chan $data]
        return
    }

    # EOF
    set code [catch {
        # Need to make the channel blocking else no error is returned
        # on the close
        fileevent $chan readable {}
        fconfigure $chan -blocking 1
        close $chan
    } result edict]
    if {$code} {
        Settle $prom $code $result $edict
    } else {
        Settle $prom 0 $data
    }
}

proc promise::pexec {args} {
    return [Promise new [lambda {open_args prom} {
        set chan [open |$open_args r]
        fconfigure $chan -blocking 0
        fileevent $chan readable [list promise::ReadChannel $prom $chan ""]
    } $args]]
}        

proc promise::Settle {prom code result {edict {}}} {
    if {![info object isa object $prom]} {
        # The object has been deleted. Naught to do
        return
    }
    if {$code == 0} {
        # OK
        $prom resolve $result
    } else {
        # TBD - how should codes other than 1 (error) be handled?
        $prom reject [list $result $edict]
    }
}
        
proc promise::ptask {script} {
    uplevel #0 package require Thread
    proc [namespace current]::ptask script { 
        return [Promise new [lambda {script prom} {
            set thread_script [string map [list %PROM% $prom %TID% [thread::id] %SCRIPT% $script] {
                thread::send -async %TID% [list ::promise::Settle %PROM% [catch {%SCRIPT%} result edict] $result $edict]
            }]
            thread::create $thread_script
        } $script]]
    }
    tailcall [namespace current]::ptask $script
}

proc promise::pworker {tpool script} {
    # No need for package require Thread since if tpool is passed to
    # us, Thread must already be loaded
    return [Promise new [lambda {tpool script prom} {
        set thread_script [string map [list %PROM% $prom %TID% [thread::id] %SCRIPT% $script] {
            thread::send -async %TID% [list ::promise::Settle %PROM% [catch {%SCRIPT%} result edict] $result $edict]
        }]
        tpool::post -detached -nowait $tpool $thread_script
    } $tpool $script]]
}
