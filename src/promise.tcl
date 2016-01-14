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
    #  CHAINED  - The promise is attached to another promise
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
        # object's settle methods [fulfill], [chain] or
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
        # 'FULFILLED', 'REJECTED' or 'CHAINED'
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
    
    method FulfillAttached {value} {
        if {$_state ne "CHAINED"} {
            return
        }
        set _value $value
        set _state FULFILLED
        my ScheduleHandlers
        return
    }
    
    method RejectAttached {erval} {
        if {$_state ne "CHAINED"} {
            return
        }
        set _value $erval
        set _state REJECTED
        my ScheduleHandlers
        return
    }
    
    # Method to invoke to fulfil a promise with a value or another promise.
    method fulfill {value} {
        # Fulfills the promise
        #   value - the value with which the promise is fulfilled
        #
        # Returns '0' if promise had already been settled and '1' if
        # it was fulfilled by the current call.

        #ruff
        # If the promise has already been settled, the method has no effect.
        if {$_state ne "PENDING"} {
            return 0;             # Already settled
        }
        
        #ruff
        # Otherwise, it is transitioned to the 'FULFILLED' state with
        # the value specified by $value. If there are any fulfillment
        # handlers registered by the [done] or [then] methods, they
        # are scheduled to be run.
        set _value $value
        set _state FULFILLED
        my ScheduleHandlers
        return 1
    }

    # Method to invoke to fulfil a promise with a value or another promise.
    method chain {promise} {
        # Chains the promise to another promise
        #   promise - the [Promise] object to which this promise is to
        #     be chained
        #
        # Returns '0' if promise had already been settled and '1' otherwise.

        #ruff
        # If the promise has already been settled, the method has no effect.
        if {$_state ne "PENDING"} {
            return 0;
        }

        #ruff
        # Otherwise, it is chained to $promise so that it reflects that
        # other promise's state.
        if {[catch {
            $promise done [namespace code {my FulfillAttached}] [namespace code {my RejectAttached}]
        } msg edict]} {
            my reject [list $msg $edict]
        } else {
            set _state CHAINED
        }
        
        return 1
    }

    method reject {errval} {
        # Rejects the promise
        #   errval - the value with which the promise is rejected.
        #     By convention, $errval should be a list consisting of an error
        #     message and error dictionary.
        #
        # Returns '0' if promise had already been settled and '1' if
        # it was rejected by the current call.

        #ruff
        # If the promise has already been settled, the method has no effect.
        if {$_state ne "PENDING"} {
            return 0;             # Already settled
        }

        #ruff
        # Otherwise, it is transitioned to the 'REJECTED' state with
        # the value specified by $errval. If there are any reject
        # handlers registered by the [done] or [then] methods, they
        # are scheduled to be run.
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

    method done {on_fulfill {on_reject {}}} {
        # Registers callback handlers to be run when the promise is settled
        #  on_fulfill - command prefix for the callback handler to run
        #    if the promise is fulfilled. If an empty string, no fulfill
        #    handler is registered.
        #  on_reject - command prefix for the callback handler to run
        #    if the promise is rejected. If unspecified or an empty string,
        #    no reject handler is registered.
        # Both handlers are called with an additional argument which is
        # the value with which the promise was settled.
        # 
        # The command may be called multiple times to register multiple
        # handlers to be run at promise settlement. If the promise was
        # already settled at the time the call was made, the handlers
        # are invoked immediately. In all cases, handlers are not called
        # directly, but are invoked by scheduling through the event loop.
        #
        # The method triggers garbage collection of the object if the
        # promise has been settled and registered handlers have been
        # scheduled. Applications can hold on to the object through
        # appropriate use of the [ref] and [unref] methods.
        #

        # TBD - as per the Promise/A+ spec, errors in done should generate
        # a background error (unlike then).
        lappend _handlers [list $on_fulfill $on_reject]
        # In case promise already fulfilled, we will need to run the handlers
        my ScheduleHandlers

        #ruff
        # The method does not return a value.
        return
    }
    
    method then {on_fulfill {on_reject {}}} {
        # Registers callback handlers to be run when the promise is settled
        # and returns a new [Promise] object that will be settled by the
        # handlers.
        #  on_fulfill - command prefix for the callback handler to run
        #    if the promise is fulfilled. If an empty string, no fulfill
        #    handler is registered.
        #  on_reject - command prefix for the callback handler to run
        #    if the promise is rejected. If unspecified or an empty string,
        #    no reject handler is registered.
        # Both handlers are called with an additional argument which is
        # the value with which the promise was settled.
        # 
        # The command may be called multiple times to register multiple
        # handlers to be run at promise settlement. If the promise was
        # already settled at the time the call was made, the handlers
        # are invoked immediately. In all cases, handlers are not called
        # directly, but are invoked by scheduling through the event loop.
        #
        # If the handler that is invoked runs without error, its return
        # value fulfills the new promise returned by the 'then' method.
        # If it raises an exception, the new promise will be rejected
        # with the error message and dictionary from the exception.
        #
        # Alternatively, the handlers can explicitly invoke commands
        # [then_fulfill], [then_reject] or [then_chain] to
        # resolve the returned promise. In this case, the return value
        # (including exceptions) from the handlers are ignored.
        #
        # If 'on_fulfill' (or 'on_reject') is an empty string (or unspecified),
        # the new promise is created and fulfilled (or rejected) with
        # the same value that would have been passed in to the handlers.
        #
        # The method triggers garbage collection of the object if the
        # promise has been settled and registered handlers have been
        # scheduled. Applications can hold on to the object through
        # appropriate use of the [ref] and [unref] methods.
        #
        return [[self class] new [list apply [list {predecessor on_fulfill on_reject prom} {
            $predecessor done \
                 [list ::promise::_then_handler $prom FULFILLED $on_fulfill] \
                 [list ::promise::_then_handler $prom REJECTED $on_reject]
        }] [self] $on_fulfill $on_reject]]
    }
}

proc promise::_then_handler {target_promise status cmd value} {
    # Run the specified command and fulfill/reject the target promise
    # accordingly. If the command is empty, the passed-in value is passed
    # on to the target promise.

    # IMPORTANT!!!!
    # MUST BE CALLED FROM EVENT LOOP AT so info level must be 1. Else
    # promise::fulfill/reject will not work
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
            FULFILLED { $target_promise fulfill $value }
            REJECTED  { $target_promise reject $value }
            CHAINED -
            PENDING  -
            default {
                $target_promise reject [promise::_make_errval PROMISE THEN STATE "Internal error: invalid status $state"]
            }
        }
    } else {
        # Invoke the real handler code and fulfill/reject the target promise.
        # Not the handler code may have called one of the promise::then_*
        # commands itself in which case these calls will be no-ops.
        # TBD - ideally we would like to execute at global level. However
        # the then_* commands retrieve target_promise from level 1 (here).
        # So directly invoke.
        if {[catch [linsert $cmd end $value] value edict]} {
            $target_promise reject [list $value $edict]
        } else {
            $target_promise fulfill $value
        }
    }
    return
}

proc promise::then_fulfill {value} {
    # Fulfills the promise returned by a [then] method call from
    # within its callback handler
    #  value - the value with which to fulfill the promise
    #
    # The [Promise.then] method is a mechanism to chain asynchronous
    # callbacks by registering them on a promise. It returns a new
    # promise which is settled by the return value from the handler,
    # or by the handler calling one of three commands - 'then_fulfill',
    # [then_reject] or [then_promise]. Calling 'then_fulfill' fulfills
    # the promise returned by the 'then' method that queued the currently
    # running callback.
    #
    # It is an error to call this command from outside a callback
    # that was queued via the [then] method on a promise.
    
    # TBD - what if someone calls this from within a uplevel #0 ? The
    # upvar will be all wrong
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_fulfill called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise fulfill $value
}

proc promise::then_chain {promise} {
    # Chains the promise returned by a [then] method call to
    # another promise
    #  promise - the promise to which the promise returned by [then] is
    #     to be chained
    #
    # The [Promise.then] method is a mechanism to chain asynchronous
    # callbacks by registering them on a promise. It returns a new
    # promise which is settled by the return value from the handler,
    # or by the handler calling one of three commands - [then_fulfill],
    # 'then_reject' or [then_promise]. Calling 'then_chain' chains
    # the promise returned by the 'then' method that queued the currently
    # running callback to $promise so that the former will be settled
    # based on the latter.
    #
    # It is an error to call this command from outside a callback
    # that was queued via the [then] method on a promise.
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_chain called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise chain $promise
}

proc promise::then_reject {errval} {
    # Rejects the promise returned by a [then] method call from
    # within its callback handler
    #  errval - the value with which to fulfill the promise
    #
    # The [Promise.then] method is a mechanism to chain asynchronous
    # callbacks by registering them on a promise. It returns a new
    # promise which is settled by the return value from the handler,
    # or by the handler calling one of three commands - [then_fulfill],
    # 'then_reject' or [then_promise]. Calling 'then_reject' rejects
    # the promise returned by the 'then' method that queued the currently
    # running callback.
    #
    # It is an error to call this command from outside a callback
    # that was queued via the [then] method on a promise.
    upvar #1 target_promise target_promise
    if {![info exists target_promise]} {
        set msg "promise::then_reject called in invalid context."
        throw [list PROMISE THEN FULFILL NOTARGET $msg] $msg
    }
    $target_promise reject $errval
}

proc promise::all {promises} {
    # Returns a promise that fulfills or rejects when all promises
    # in the $promises argument have fulfilled or any one has rejected
    # promises - a list of Promise objects
    #
    # If any of $promises rejects, then the promise returned by the
    # command will reject with the same value. Otherwise, the promise
    # will fulfill when all promises have fulfilled.
    # The resolved value will be a list of the resolved
    # values of the contained promises.
    
    set all_promise [Promise new [lambda {promises prom} {
        if {[llength $promises] == 0} {
            $prom fulfill {}
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
    # Returns a promise that fulfills or rejects when all promises
    # in the $args argument have fulfilled or any one has rejected
    # args - list of Promise objects
    # This command is identical to the all command except that it takes
    # multiple arguments, each of which is a Promise object. See [all]
    # for a description.
    return [all $args]
}

# Callback for promise::all.
#  all_promise - the "master" promise returned by the all call.
#  remaining_promises - the list of remaining promises still to be resolved.
#  values - values collected so far from fulfilled promises
#  resolution - whether the current promise was resolved with "FULFILLED"
#   or "REJECTED"
#  value - the value of the currently fulfilled promise or error description
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
        $all_promise fulfill $values
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
    # Returns a promise that fulfills or rejects when any promise
    # in the $promises argument is fulfilled or rejected
    #   promises - a list of Promise objects
    # The returned promise will fulfill and reject with the same value
    # as the first promise in $promises that fulfills or rejects.
    set race_promise [Promise new [lambda {promises prom} {
        if {[llength $promises] == 0} {
            $prom reject [_make_errval PROMISE RACE EMPTYSET "Promise set is empty"]
            return
        }
        foreach promise $promises {
            $promise done [list $prom fulfill] [list $prom reject]
        }
    } $promises]]

    return $race_promise
}

proc promise::race* {args} {
    # Returns a promise that fulfills or rejects when any promise
    # in the passed arguments is fulfilled or rejected
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
    # Returns a promise that will be fulfilled when the specified URL is fetched
    #   url - the URL to fetch
    #   args - arguments to pass to the 'http::geturl' command
    # This command invokes the asynchronous form of the 'http::geturl' command
    # of the 'http' package. If the operation completes with a status of
    # 'ok', the returned promise is fulfilled with the contents of the
    # http state array (see the documentation of http::geturl). If the
    # the status is anything else, the promise is rejected, again with
    # the contents of the http state array.
    uplevel #0 {package require http}
    proc [namespace current]::pgeturl {url args} {
        set prom [promise::Promise new [lambda {http_args prom} {
            http::geturl {*}$http_args -command [promise::lambda {prom tok} {
                upvar #0 $tok http_state
                if {$http_state(status) eq "ok"} {
                    $prom fulfill [array get http_state]
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

proc promise::ptimer {millisecs {value "Timer expired."}} {
    # Returns a promise that will be fulfilled when the specified time has
    # elapsed
    #  millisecs - time interval in milliseconds
    #  value - the value with which the promise is to be fulfilled
    # In case of errors (e.g. if $milliseconds is not an integer), the
    # promise is rejected with an error value consisting of the error message
    # and an error dictionary.
    # Also see [ptimeout] which is similar but rejects the promise instead
    # of fulfilling it.
    return [promise::Promise new [lambda {millisecs prom} {
        after $millisecs [list $prom fulfill $value]
    } $millisecs]]
}

proc promise::ptimeout {millisecs {value "Operation timed out."}} {
    # Returns a promise that will be rejected when the specified time has
    # elapsed
    #  millisecs - time interval in milliseconds
    #  value - the value with which the promise is to be rejected
    # In case of errors (e.g. if $milliseconds is not an integer), the
    # promise is rejected with an error value consisting of the error message
    # and an error dictionary.
    # Also see [ptimer] which is similar but fulfills the promise instead
    # of rejecting it.
    return [promise::Promise new [lambda {millisecs prom} {
        after $millisecs [list $prom reject $value]
    } $millisecs]]
}

proc promise::pconnect {args} {
    # Returns a promise that will be fulfilled when the a socket connection
    # is completed
    #  args - arguments to be passed to the Tcl 'socket' command
    # This is a wrapper for the async version of the Tcl 'socket' command.
    # If the connection completes, the promise is fulfilled with the
    # socket handle.
    # In case of errors (e.g. if the address cannot be fulfilled), the
    # promise is rejected with an error value consisting of the error message
    # and an error dictionary.
    # 
    return [Promise new [lambda {so_args prom} {
        set so [socket -async {*}$so_args]
        fileevent $so writable [promise::lambda {prom so} {
            fileevent $so writable {}
            $prom fulfill $so
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
    # Runs an external program and returns a promise for its output
    # a promise for the script results
    #  args - program and its arguments as passed to the Tcl 'open' call
    #    for creating pipes
    # If the program runs without errors, the promise is fulfilled by its
    # standard output content. Otherwise the promise is rejected with
    # an error value consisting of an error message and dictionary
    # detailing the failure.
    #
    # Returns a promise that will be settled by the result of the program
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
        $prom fulfill $result
    } else {
        # TBD - how should codes other than 1 (error) be handled?
        $prom reject [list $result $edict]
    }
}
        
proc promise::ptask {script} {
    # Creates a new Tcl thread to run the specified script and returns
    # a promise for the script results
    #   script - script to run in the thread
    # Returns a promise that will be settled by the result of the script
    #
    # The `ptask` command runs the specified script in a new Tcl
    # thread. The promise returned from this command will be fulfilled
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
    # returned from this command will be fulfilled with the
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

        In this package, the [::promise::Promise] class implements promises.
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
        times, it can use the 'ref' and 'unref' methods of a
        [::promise::Promise] object to explicitly manage its lifetime.
        
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
                $prom fulfill [list $http_state(url) $http_state(status)]
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
