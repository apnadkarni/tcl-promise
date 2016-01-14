#
# Copyright (c) 2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file license.terms for license

namespace eval promise {
    proc version {} { return 0.2 }
}

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
        # Create a promise for the asynchronous operation to be initiated
        # by $cmd.
        # cmd - a command prefix that should initiate an asynchronous
        #  operation.
        # The command prefix $cmd is passed an additional argument - the
        # name of this Promise object. It should arrange for one of the
        # object's settle methods [resolve], [resolve_with_promise] or
        # [reject] to be called when the operation completes.
        
        set _state PENDING
        set _handlers [list ]
        set _nrefs 0
        
        # Errors in the construction command are returned via
        # the standard mechanism of reject.
        if {[catch {
            uplevel #0 [linsert $cmd end [self]]
        } msg edict]} {
            my reject [list $msg $edict]
        }
    }

    destructor {
        # Destroys the object.
        #
        # This method should not be generally called directly as [Promise]
        # objects are garbage collected either automatically or via the [ref]
        # and [unref] methods.
    }
    
    method state {} {
        # Returns the current state of the promise
        #
        # The promise state may be one of the values 'PENDING',
        # 'FULFILLED', 'REJECTED' or 'ATTACHED'
        return $_state
    }
    
    method value {} {
        # Returns the settled value for the promise
        #
        # The returned value may be the fulfilled value or the rejected
        # value depending on whether the associated operation was successfully
        # completed or failed.
        #
        # An error is raised if the promise is not settled yet.
        if {$_state ni {FULFILLED REJECTED}} {
            error "Value is not set."
        }
        return $_value
    }

    method ref {} {
        # Increments the reference count for the object
        incr _nrefs
    }

    method unref {} {
        # Decrements the reference count for the object
        #
        # The object may have been destroyed when the call returns.
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
    # Returns a promise that resolves or rejects when all promises
    # in the $promises argument have resolved or any one has rejected
    # promises - a list of Promise objects
    #
    # If any of $promises rejects, then the promise returned by the
    # command will reject with the same value. Otherwise, the promise
    # will resolve when all promises have resolved.
    # The resolved value will be a list of the resolved
    # values of the contained promises.
    
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

proc promise::all* args {
    # Returns a promise that resolves or rejects when all promises
    # in the $args argument have resolved or any one has rejected
    # args - list of Promise objects
    # This command is identical to the all command except that it takes
    # multiple arguments, each of which is a Promise object. See [all]
    # for a description.
    return [all $args]
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
    # TBD - this does not seem the best way to do this. In particular,
    # an earlier promise might stay pending though a later promise has
    # rejected causing us to wait unnecesarily
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
        return
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
    # Returns a promise that resolves or rejects when any promise
    # in the $promises argument is resolved or rejected
    #   promises - a list of Promise objects
    # The returned promise will resolve and reject with the same value
    # as the first promise in $promises that resolves or rejects.
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

proc promise::race* {args} {
    # Returns a promise that resolves or rejects when any promise
    # in the passed arguments is resolved or rejected
    #   args - list of Promise objects
    # This command is identical to the all command except that it takes
    # multiple arguments, each of which is a Promise object. See [race]
    # for a description.
    return [race $args]
}

proc promise::_make_errval {args} {
    catch {throw $args [lindex $args end]} msg edict
    return [list $msg $edict]
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

proc promise::_read_channel {prom chan data} {
    set newdata [read $chan]
    if {[string length $newdata] || ![eof $chan]} {
        append data $newdata
        fileevent $chan readable [list [namespace current]::_read_channel $prom $chan $data]
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
        _settle $prom $code $result $edict
    } else {
        _settle $prom 0 $data
    }
}

proc promise::pexec {args} {
    return [Promise new [lambda {open_args prom} {
        set chan [open |$open_args r]
        fconfigure $chan -blocking 0
        fileevent $chan readable [list promise::_read_channel $prom $chan ""]
    } $args]]
}        

proc promise::_settle {prom code result {edict {}}} {
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
    # Creates a new Tcl thread to run the specified script and returns
    # a promise for it
    #   script - script to run in the thread
    # Returns a promise that will be settled by the result of the script
    #
    # The `ptask` command runs the specified script in a new Tcl
    # thread. The promise returned from this command will be resolved
    # with the result of the script if it completes
    # successfully. Otherwise, the promise will be rejected with an
    # error value that is a pair containing the error message and
    # error dictionary from the script failure.
    #
    # The command requires the Thread package to be loaded.

    uplevel #0 package require Thread
    proc [namespace current]::ptask script { 
        return [Promise new [lambda {script prom} {
            set thread_script [string map [list %PROM% $prom %TID% [thread::id] %SCRIPT% $script] {
                thread::send -async %TID% [list ::promise::_settle %PROM% [catch {%SCRIPT%} result edict] $result $edict]
            }]
            thread::create $thread_script
        } $script]]
    }
    tailcall [namespace current]::ptask $script
}

proc promise::pworker {tpool script} {
    # Runs a script in a worker thread from a thread pool and returns a promise for the same
    #   tpool - thread pool identifier
    #   script - script to run in the worker thread
    # Returns a promise that will be settled by the result of the script
    #
    # The Thread package allows creation of a thread pool
    # with the 'tpool create' command. The `pworker` command runs the
    # specified script in a worker thread from a thread pool. The promise
    # returned from this command will be resolved with the
    # result of the script if it completes successfully. Otherwise,
    # the promise will be rejected with an error value that is a pair
    # containing the error message and error dictionary from the script
    # failure.

    
    # No need for package require Thread since if tpool is passed to
    # us, Thread must already be loaded
    return [Promise new [lambda {tpool script prom} {
        set thread_script [string map [list %PROM% $prom %TID% [thread::id] %SCRIPT% $script] {
            thread::send -async %TID% [list ::promise::_settle %PROM% [catch {%SCRIPT%} result edict] $result $edict]
        }]
        tpool::post -detached -nowait $tpool $thread_script
    } $tpool $script]]
}

proc promise::_document_self {path args} {
    # Generates documentation for the package in HTML format.
    # path - path to the output file

    package require ruff

    set intro {
        This package implements the *promise* abstraction for
        asynchronous programming. This document is the reference for
        commands and classes implemented by the package. For a
        tutorial introduction to promises, usage guide and examples, see
        http://www.magicsplat.com/blog/promises.
    }

    set abstraction {
        The promise abstraction encapsulates the eventual result of a
        possibly asynchronous operation.

        This package follows the terminology used in the Javascript world,
        in particular the ECMAScript 2015 Language specification though
        details of implementation differ.
        
        From an application's perspective, a Promise object may be in one
        of three states:

        - FULFILLED
        - REJECTED
        - PENDING, if it is neither of the above

        Though the above specification does not explicitly assign meanings
        to these states, in practice FULFILLED and REJECTED are associated
        with successful and failed completion of the operation respectively
        while PENDING reflects the operation has not completed.

        Some additional terms:

        A promise is said to be settled if it is either in the FULFILLED
        or REJECTED state. A promise that is settled will thereafter never
        change state.

        A promise is said to be resolved if it is settled or if it
        is attached to the state of another promise.

        In this package, the Promise class implements promises.
    }
    
    set gc {
        TclOO objects are not garbage collected and have to be explicitly
        destroyed. In the case of promises, because of their asynchronous
        nature, it is often not clear to applications when the promise
        objects should be destroyed.

        Therefore the package internally manages the lifetime of Promise
        objects such that they are automatically destroyed once they are
        settled and at least one fulfillment or rejection handler has been
        run. This removes the burden from the application in the most common
        usage scenarios. In cases where the application wants the object to
        persist, for example, when the resolved value is accessed multiple
        times, it can use the 'ref' and 'unref' methods of the
        [::promise::Promise] object to explicitly manage the lifetime of the
        object.
        
    }
    
    ::ruff::document_namespaces html [namespace current] \
        -autolink false \
        -recurse true \
        -output $path \
        -titledesc "promise (V[version])" \
        -copyright "[clock format [clock seconds] -format %Y] Ashok P. Nadkarni" \
        {*}$args \
        -preamble [dict create :: \
                       [list \
                            Introduction [::ruff::extract_docstring $intro] \
                            {The promise abstraction} [::ruff::extract_docstring $abstraction] \
                            {Garbage collection} [::ruff::extract_docstring $gc] \
                           ]]
}

if {0} {
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

    [promise::all [
                   list [
                         promise::ptask {expr 1+1}
                        ] [
                           promise::ptask {expr 2+2}
                          ]
                  ]] done [promise::lambda val {puts [tcl::mathop::* {*}$val]}] 
}

package provide promise [promise::version]

if {[info exists ::argv0] &&
    [file tail [info script]] eq [file tail $::argv0]} {
    switch -glob -- [lindex $::argv 0] {
        ver* { puts [promise::version] }
        tm -
        dist* {
            if {[file extension [info script]] ne ".tm"} {
                file copy -force [info script] [file rootname [info script]]-[promise::version].tm
            } else {
                error "Cannot create distribution from a .tm file"
            }
        }
        doc* {
            promise::_document_self promise-[promise::version].html
        }
        default {
            error "Unknown option/command \"[lindex $::argv 0]\""
        }
    }
}
