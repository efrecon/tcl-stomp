package require Tcl
package require md5

namespace eval ::stomp {
    variable STOMP
    if {![info exists STOMP] } {
	array set STOMP {
	    versions       {1.0 1.1 1.2}
	    idGene         0
	    idClamp        10000
	    idFormat       7
	    verbose        0
	    dbgfd          stderr
	    dateLogHeader  "\[%Y%m%d %H%M%S\] "
	}
	variable version 0.2
	variable libdir [file dirname [file normalize [info script]]]
    }
}


# ::stomp::UpVar -- Find true caller
#
#       Finds how many stack levels there are between the direct
#       caller to this procedure and the true caller of that caller,
#       accounting for indirection procedures aiming at making
#       available some of the local procedures from this namespace to
#       child namespaces.
#
# Arguments:
#	None.
#
# Results:
#       Number of levels to jump up the stack to access variables as
#       if upvar 1 had been used in regular cases.
#
# Side Effects:
#       None.
proc ::stomp::UpVar {} {
    set signature [info level -1]
    for { set caller -2} { $caller>-10 } { incr caller -1} {
	if { [info level $caller] eq $signature } {
	    return [expr {-$caller}]
	}
    }
    return 1
}


# ::stomp::GetOpt -- Quick and Dirty Options Parser
#
#       Parses options, code comes from wiki
#
# Arguments:
#	arg1	descr
#	arg2	descr
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::GetOpt {_argv name {_var ""} {default ""}} {
    upvar [UpVar] $_argv argv $_var var
    set pos [lsearch -regexp $argv ^$name]
    if {$pos>=0} {
	set to $pos
	if {$_var ne ""} {
	    set var [lindex $argv [incr to]]
	}
	set argv [lreplace $argv $pos $to]
	return 1
    } else {
	# Did we provide a value to default?
	if {[llength [info level 0]] == 5} {set var $default}
	return 0
    }
}



# ::stomp::Debug -- Conditional debug output
#
#       Output debug message depending on the debug level that is
#       currently associated to the library.  Debug occurs on the
#       registered file descriptor.
#
# Arguments:
#	lvl	Debug level of message, lib. level must be lower for input
#	output	Message to write out, possibly
#
# Results:
#       None.
#
# Side Effects:
#       Write message onto debug file descriptor, if applicable.
proc ::stomp::Debug { lvl output } {
    variable STOMP

    if { $STOMP(verbose) >= $lvl } {
	set hdr [clock format [clock seconds] -format $STOMP(dateLogHeader)]
	puts $STOMP(dbgfd) ${hdr}${output}
    }
}


proc ::stomp::Identifier {} {
    variable STOMP

    set unique [incr STOMP(idGene)]
    append unique [expr {[clock clicks -milliseconds]%$STOMP(idClamp)}]
    return [format "%.$STOMP(idFormat)d" $unique]
}


proc ::stomp::DecideBeat { local remote } {
    variable STOMP
    
    if { $local > 0 && $remote > 0 } {
	if { $local > $remote } {
	    return $local
	} else {
	    return $remote
	}
    }
    return 0
}


proc ::stomp::verbosity { {lvl -1} } {
    variable STOMP
    
    if { $lvl >= 0 } {
	set STOMP(verbose) $lvl
    }
    return $STOMP(verbose)
}


package provide stomp $::stomp::version
