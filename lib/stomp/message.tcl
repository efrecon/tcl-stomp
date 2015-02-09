##################
## Module Name     --  stomp::message
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Provide low-level messaging facilities to construct, send and
##    receive messages; including heart-beating and keep-alive
##    whenever relevant.
##
##################

package require Tcl

namespace eval ::stomp::message {
    variable STOMP
    if {![info exists STOMP] } {
	array set STOMP {
	    client   {CONNECT STOMP SEND MESSAGE ERROR
		SUBSCRIBE UNSUBSCRIBE BEGIN COMMIT ABORT
		ACK NACK DISCONNECT}
	    server   {CONNECTED MESSAGE RECEIPT ERROR}
	    -headers  {
		CONNECT {
		    required { accept-version host }
		    optional { login passcode heart-beat }
		    body 0
		}
		STOMP {
		    required { accept-version host }
		    optional { login passcode heart-beat }
		    body 0
		}
		CONNECTED {
		    required { version }
		    optional { session server heart-beat }
		    body 1
		}
		SEND {
		    required { destination }
		    optional { transaction * }
		    body 1
		}
		SUBSCRIBE {
		    required { destination id }
		    optional { ack }
		    body 0
		}
		UNSUBSCRIBE {
		    required { id }
		    optional { }
		    body 0
		}
		ACK {
		    required { id }
		    optional { transaction }
		    body 0
		}
		NACK {
		    required { id }
		    optional { transaction }
		    body 0
		}
		BEGIN {
		    required { transaction }
		    optional { }
		    body 0
		}
		COMMIT {
		    required { transaction }
		    optional { }
		    body 0
		}
		ABORT {
		    required { transaction }
		    optional { }
		    body 0
		}
		DISCONNECT {
		    required { }
		    optional { receipt }
		    body 0
		}
		MESSAGE {
		    required { destination message-id subscription }
		    optional { ack * }
		    body 1
		}
		RECEIPT {
		    required { receipt-id }
		    optional { }
		    body 1
		}
		ERROR {
		    required { }
		    optional { message }
		    body 1
		}
	    }
	    -type    client
	    -strict  off
	    -max-len 10485760
	    charset  utf-8
	    types    {msg reader heartbeat keepalive factory}
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]

	# Bridge some local procedures from main namespace into here
	# so we can use them without having to export them.
	foreach cmd [list Debug Identifier GetOpt] {
	    namespace eval [namespace current] \
		[string map [list @cmd@ $cmd] {
		    proc @cmd@ { args } {
			namespace eval [namespace parent] @cmd@ $args
		    }
		}]
	}

    }
}

proc ::stomp::message::IsA { varname type } {
    variable STOMP

    set idx [lsearch -glob $STOMP(types) $type]
    if { $idx < 0 } {
	return -code error "$type is not a known type: should be one of\
                            [join $STOMP(types) ", "]"
    }
    set type [lindex $STOMP(types) $idx]
    if { [lsearch [info vars [namespace current]::${type}_*] $varname]<0 } {
	return 0
    }
    return 1
}



proc ::stomp::message::contentType { {mimeType "text/plain"} {charset ""} } {
    variable STOMP

    if { $charset eq "" } {
	# XXX: Wrong, only if starting with text/ ??
	set charset $STOMP(charset)
    }

    # Shall we perform any syntax check on the mimeType and/or the
    # charset??
    return "${mimeType};charset=${charset}"
}


proc ::stomp::message::Error { reader type { msg "" } } {
    variable STOMP

    if { ![IsA $reader reader] } {
	return -code error "$reader is not a valid input reader context"
    }

    upvar \#0 $reader R
    upvar \#0 $R(factory) FCTRY

    # Stop listening for incoming data and cleanup
    fileevent $FCTRY(sock) readable ""
    InitReader $reader 0

    # Tell callers we've had an error, non-recoverable.
    if { $R(error) ne "" } {
	if { [catch {eval [linsert $R(error) end $type "$msg"]} res] } {
	    Debug 3 \
		"Error when callbacking error dispatcher: $res"
	}
    }

}


proc ::stomp::message::InitReader { reader { keep 1 } } {
    upvar \#0 $reader R

    if { [string is false $keep] && [llength [array names R msg]]>0 \
	     && $R(msg) ne "" } {
	delete $R(msg)
    }

    set R(msg) ""
    set R(state) "START"
    set R(len) -1;    # No target length yet.
    set R(bdySize) 0; # Nothing read yet
}


proc ::stomp::message::HandleInput { reader } {
    variable STOMP

    if { ![IsA $reader reader] } {
	return -code error "$reader is not a valid input reader context"
    }

    upvar \#0 $reader R
    upvar \#0 $R(factory) FCTRY

    if { [eof $FCTRY(sock)] } {
	Error $reader EOF "EOF reached, connection lost"
    } else {
	# Detect end of message, getting rid of the final \0 from the
	# end the line.
	set end 0
	if { $R(len) > 0 } {
	    set line [read $FCTRY(sock) [expr {$R(len)-$R(bdySize)+1}]]
	    set size [string length $line]

	    incr R(bdySize) $size
	    if { $R(bdySize) >= $R(len) } {
		set line [string range $line 0 end-1]
		set end 1
	    }
	} else {
	    set size [gets $FCTRY(sock) line]
	    set end [regsub -all \x00 $line "" line]
	}

	# Remember we received data from socket.
	heartBeat $FCTRY(sock)

	# Now read the bits and pieces of the message, 
	switch $R(state) {
	    "START" {
		# Advance only if not just an heart-beat
		if { [string trim $line] ne "" } {
		    set R(msg) [new [string toupper [string trim $line]]]
		    set R(state) HEADER
		} elseif { $end } {
		    Debug 4 "Received beat from remote"
		}
	    }
	    "HEADER" {
		if { $line ne "" } {
		    foreach {k v} [split $line ":"] break
		    setHeader $R(msg) $k $v
		} else {
		    upvar \#0 $R(msg) M
		    set R(len) [getHeader $R(msg) content-length -1]
		    if { ! [AllowLength $R(msg) $R(len)] } {
			Error $reader MAXLEN "Maxmimum message size of\
                                              $FCTRY(-max-len) bytes would be\
                                              overtramped."
		    }
		    set R(bdySize) 0
		    set R(state) BODY
		    set R(translation) [fconfigure $FCTRY(sock) -translation]
		    set R(encoding) [fconfigure $FCTRY(sock) -encoding]
		    # Always enter binary mode
		    fconfigure $FCTRY(sock) \
			-translation [list binary [lindex $R(translation) 1]] \
			-encoding binary
		}
	    }
	    "BODY" {
		if { $line eq "" && $R(len) < 0 } {
		    set end 1
		} else {
		    setBody $R(msg) $line 1
		}
	    }
	}

	if { $end && $R(state) eq "BODY" } {
	    fconfigure $FCTRY(sock) \
		-translation $R(translation) \
		-encoding $R(encoding)
	    if { $R(msg) ne "" } {
		if { [catch {eval [linsert $R(handler) end $R(msg)]} res] } {
		    Debug 3 \
			"Error when callbacking message dispatcher: $res"
		}
	    }

	    # Done, start waiting for next message, deleting the
	    # message is up to the caller.
	    InitReader $reader
	}
    }
}


proc ::stomp::message::delete { varName } {
    variable STOMP
    
    foreach {type id} [split [namespace tail $varName] "_"] break
    switch $type {
	"factory" {
	    if { ![IsA $varName factory] } {
		return -code error \
		    "$varName is not a valid factory"
	    }

	    upvar \#0 $varName FCTRY
	    # Delete all messages associated to factory
	    foreach msg [info vars [namespace current]::msg_$FCTRY(id)_*] {
		delete $msg
	    }
	    unset $varName
	}
	"reader" {
	    if { ![IsA $varName reader] } {
		return -code error \
		    "$varName is not a valid input reader context"
	    }

	    upvar \#0 $varName R
	    upvar \#0 $R(factory) FCTRY
	    # Stop listening for incoming data
	    fileevent $FCTRY(sock) readable ""
	    InitReader $varName 0;  # Will delete message if any
	    unset $varName
	}
	"msg" {
	    if { ![IsA $varName msg] } {
		return -code error \
		    "$varName is not an existing message (anymore?)"
	    }
	    unset $varName
	}
	"heartbeat" {
	    if { ![IsA $varName heartbeat] } {
		return -code error \
		    "$varName is not a valid heart-beat context"
	    }
	    upvar \#0 $varName HB

	    heartBeat $HB(sock) -1
	}
	"keepalive" {
	    if { ![IsA $varName keepalive] } {
		return -code error \
		    "$varName is not a valid keep-alive context"
	    }
	    upvar \#0 $varName KA

	    keepAlive $KA(sock) -1
	}
	default {
	    return -code error \
		"$varName identifies neither a reader, neither a message"
	}
    }
}


proc ::stomp::message::reader { factory handlerCmd { errorCmd "" } } {
    variable STOMP

    if { ![IsA $factory factory] } {
	return -code error "$factory is not a known message factory"
    }
    upvar \#0 $factory FCTRY

    # Create container array for message and initialise it
    set reader [namespace current]::reader_$FCTRY(id)_[Identifier]
    upvar \#0 $reader R

    set R(factory) $factory
    set R(handler) $handlerCmd
    set R(error) $errorCmd
    InitReader $reader

    fileevent $FCTRY(sock) readable \
	[list [namespace current]::HandleInput $reader]
    
    return $reader
}


proc ::stomp::message::Charset { msg } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }

    upvar \#0 $msg M
    array set HDR $M(headers)

    # Extract charset= from content-type specification, if any and
    # available.
    if { [llength [array names HDR "content-type"]] > 0 } {
	set type text/plain
	set charset ""
	foreach token [split $HDR(content-type) ";"] {
	    switch -glob -nocase -- $token {
		*/* {
		    set type $token
		}
		charset*=* {
		    foreach {k charset} [split $token "="] break
		}
	    }
	}

	# Defaults to utf-8 for text types.
	if { [string match text/* $type] } {
	    if { $charset eq "" } {
		set charset $STOMP(charset)
	    }
	}
    }

    return "";    # None, binary?
}


proc ::stomp::message::KeepAlive { ka } {
    variable STOMP
    
    if { ![IsA $ka keepalive] } {
	return -code error \
	    "$ka does not identify a keep alive context (anymore)"
    }
    upvar \#0 $ka KA

    # Send a LF on sock and register next periodical sending.
    Debug 4 "Sending heart-beat"
    puts $KA(sock) ""
    if { [catch {flush $KA(sock)} err] } {
	Debug 3 "Connection lost"
    }
    keepAlive $KA(sock);  # Register next
}


proc ::stomp::message::keepAlive { sock { period 0 } } {
    variable STOMP

    foreach ka [info vars [namespace current]::keepalive_*] {
	upvar \#0 $ka KA
	if { $KA(sock) eq $sock } {
	    # Cancel timer, if any
	    if { $KA(timer) ne "" } {
		after cancel $KA(timer)
		set KA(timer) ""
	    }

	    # Set to new period if positive
	    if { $period > 0 } {
		set KA(period) $period
	    }

	    # Negative period, stop keeping alive socket.
	    if { $period < 0  } {
		unset $ka
		return ""
	    } else {
		set KA(timer) \
		    [after $KA(period) [namespace current]::KeepAlive $ka]
		return $ka
	    }
	}
    }

    if { $period > 0 } {
	# Create container array for keep alive state and initialise it
	set ka [namespace current]::keepalive_[Identifier]
	upvar \#0 $ka KA

	set KA(sock) $sock
	set KA(period) 0
	set KA(timer) ""
	return [keepAlive $sock $period]
    }
}


proc ::stomp::message::Expire { hb } {
    variable STOMP
    

    if { ![IsA $hb heartbeat]} {
	return -code error "$hb is not a valid heart-beat context"
    }
    
    upvar \#0 $hb HB
    if { [catch {eval [linsert $HB(handler) end $hb]} res] } {
	Debug 3 \
	    "Error when callbacking message dispatcher: $res"
    }
}


proc ::stomp::message::heartBeat {sock {period 0} {cbCmd ""}} {
    variable STOMP

    foreach hb [info vars [namespace current]::heartbeat_*] {
	upvar \#0 $hb HB
	if { $HB(sock) eq $sock } {
	    if { $HB(timer) ne "" } {
		after cancel $HB(timer)
		set HB(timer) ""
	    }
	    if { $cbCmd ne "" } {
		set HB(handler) $cbCmd
	    }
	    if { $period < 0 } {
		unset $hb
		return ""
	    }
	    if { $period > 0 } {
		set HB(period) $period
	    }
	    set HB(latest) [clock clicks -milliseconds]
	    set HB(timer) [after $HB(period) [namespace current]::Expire $hb]
	    
	    return $hb
	}
    }

    if { $period > 0 } {
	set hb [namespace current]::heartbeat_[Identifier]
	upvar \#0 $hb HB

	set HB(sock) $sock
	set HB(timer) ""
	return [heartBeat $sock $period $cbCmd]
    } else {
	return ""
    }
}


proc ::stomp::message::send { msg { sock "" } } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    # Use the socket from the factory (in most cases)
    if { $sock eq "" } {
	if { $M(factory) ne "" } {
	    upvar \#0 $M(factory) FCTRY
	    set sock $FCTRY(sock)
	} else {
	    return -code error "No socket to send message on!"
	}
    }

    # XX: If in a factory, shouldn't we take into account the type of
    # the factory (client or server) and refuse to SEND the commands
    # that cannot be sent from server or clients??

    # Automatigically add the length of the body to the header
    if { $M(body) ne "" } {
	set len -1
	foreach {- len} [getHeaders $msg content-length] break
	if { $len < 0 } {
	    setHeader $msg content-length [string length $M(body)]
	}
    }

    # Remember encoding and arrange to start pushing header in utf-8
    # with lf
    set translation [fconfigure $sock -translation]
    set encoding [fconfigure $sock -encoding]
    fconfigure $sock \
	-translation [list [lindex $translation 0] lf] \
	-encoding utf-8
    
    # Start by writing command and header (lf is mandatory, utf-8 for
    # all header lines). Make sure we end up with an empty line.
    puts $sock $M(command)
    Debug 4 "Sending $M(command)"
    foreach { k v } $M(headers) {
	Debug 5 "\t${k}:${v}"
	puts $sock "${k}:${v}"
    }
    puts $sock "";   # End of header marker

    # If we have a body write it...
    set charset [Charset $msg]
    fconfigure $sock \
	-translation [list [lindex $translation 0] binary] \
	-encoding [expr {$charset eq "" ? "binary" : $charset}]
    if { $M(body) ne "" } {
	puts -nonewline $sock $M(body)
    }
    
    # End of message marker.
    fconfigure $sock -translation binary -encoding binary
    puts -nonewline $sock "\0"
    
    # Restore translation and encoding
    fconfigure $sock -translation $translation -encoding $encoding

    # Physically send message to remote end, making sure we account
    # for problems when sending.
    if { [catch {flush $sock} err] == 0 } {
	keepAlive $sock;  # Keep alive socket if we should...
	return 1
    }
    return 0
}


proc ::stomp::message::GetConfig { msg cfg } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    # Force a dash
    set cfg -[string trimleft $cfg "-"]

    # Decide strict check of headers or not.
    set configuration $STOMP($cfg)
    if { $M(factory) ne "" } {
	upvar \#0 $M(factory) FCTRY
	set configuration $FCTRY($cfg)
    }
    
    return $configuration
}


proc ::stomp::message::Constraints { msg } {
    variable STOMP

    upvar \#0 $msg M

    array set CONSTRAINTS [GetConfig $msg -headers]
    if { [llength [array names CONSTRAINTS $M(command)]] == 0 } {
	return -code error "Message command $cmd is unknown!"
    }
    array set CONSTRAINT $CONSTRAINTS($M(command))

    # Automatically append content-length and content-type
    lappend CONSTRAINT(optional) "content-length"
    if { $CONSTRAINT(body) } {
	lappend CONSTRAINT(optional) "content-type"
    }

    return [array get CONSTRAINT]
}


proc ::stomp::message::CheckBody { msg } {
    variable STOMP
    
    upvar \#0 $msg M

    array set CONSTRAINT [Constraints $msg]
    return [string is true $CONSTRAINT(body)]
}

proc ::stomp::message::CheckHeader { msg { hdr "" } } {
    if { $hdr eq "" } {
	upvar \#0 $msg M

	foreach { h v } $M(headers) {
	    if { ![CheckHeader $msg $h] } {
		return 0
	    }
	}
	return 1
    }

    # Find out existing constraints for message, depending on command
    array set CONSTRAINT [Constraints $msg]

    foreach allowed $CONSTRAINT(required) {
	if { [string match -nocase $allowed $hdr] } {
	    return 1
	}
    }
    
    foreach allowed $CONSTRAINT(optional) {
	if { [string match -nocase $allowed $hdr] } {
	    return 2
	}
    }

    # If unstrict, the default, allow the key anyway.
    set strict [GetConfig $msg -strict]
    return [expr {!$strict}]
}


proc ::stomp::message::setCommand { msg cmd } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M
    
    set old $M(command)
    set M(command) [string toupper $cmd]
    if { ![CheckHeader $msg] || ![CheckBody $msg] } {
	set M(command) $old
	Debug 3 "Message type $old could not be changed to $cmd"
    }
    return $M(command)
}


proc ::stomp::message::getCommand { msg } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    return [string toupper $M(command)]
}


proc ::stomp::message::getHeaders { msg { hdr {} } } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    if { [llength $hdr] == 0 } {
	return $M(headers)
    } else {
	set result [list]
	array set HDR $M(headers)
	foreach h $hdr {
	    foreach k [array names HDR $h] {
		lappend result $k $HDR($k)
	    }
	}
	return $result
    }
}


proc ::stomp::message::getHeader { msg hdr { dft "" } } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    array set HDR $M(headers)
    foreach k [array names HDR $hdr] {
	return $HDR($k)
    }
    return $dft
}


proc ::stomp::message::SetHeader { msg hdr value } {
    variable STOMP

    upvar \#0 $msg M

    # Append header key only if not already found in the header list,
    # this serves as an implementation for
    # http://stomp.github.io/stomp-specification-1.2.html#Repeated_Header_Entries
    if { [CheckHeader $msg $hdr] } {
	set found 0
	foreach { k v } $M(headers) {
	    if { $k eq $hdr } {
		set found 1
	    }
	}
	if { $found } {
	    Debug 3 "Header key '$hdr' already present, ignoring!"
	    return 0
	} else {
	    lappend M(headers) $hdr $value
	    return 1
	}
    } else {
	Debug 2 "Header key '$hdr' not allowed for command $M(command)"
    }
    
    return 0
}

proc ::stomp::message::setHeader { msg args } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M
    
    set headers {};  # List of key/values that we have set, empty initially
    foreach { hdr value } $args {
	set hdr [string tolower [string trim $hdr]]
	if { [SetHeader $msg $hdr $value] } {
	    lappend headers $hdr $value;   # Remember positive setting
	}
    }

    return $headers
}


proc ::stomp::message::getBody { msg } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    return $M(body)
}


proc ::stomp::message::AllowLength { msg len } {
    variable STOMP

    upvar \#0 $msg M
    
    # Messages created outside of a factory have no limitations...
    if { $M(factory) eq "" } {
	return 1
    }
    
    upvar \#0 $M(factory) FCTRY
    if { $FCTRY(-max-len) > 0 && $len > $FCTRY(-max-len) } {
	return 0
    }
    return 1
}


# set/Append bdy to message body
proc ::stomp::message::setBody { msg bdy { append 0 } } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    if { [CheckBody $msg] } {
	set len [string length $bdy]
	if { [string is true $append] } {
	    set dstlen [expr {$len+[string length $M(body)]}]
	    if { ! [AllowLength $msg $dstlen] } {
		return 0
	    }
	    append M(body) $bdy
	} else {
	    if { ! [AllowLength $msg $len] } {
		return 0
	    }
	    set M(body) $bdy
	}
	return 1
    } else {
	return -code error "$M(command) does not accept any body!"
    }

    return 0
}

proc ::stomp::message::NewMessage { { factory "" } } {
    variable STOMP

    # Find the factory we should associate the message to, if any.
    if { $factory ne "" } {
	if { ![IsA $factory factory] } {	
	    return -code error "$factory is not a known message factory!"
	}
	upvar \#0 $factory FCTRY
	set msg [namespace current]::msg_$FCTRY(id)_[Identifier]
    } else {
	set msg [namespace current]::msg_[Identifier]
    }

    return $msg
}

proc ::stomp::message::dup { msg { factory "" } } {
    variable STOMP

    if { ![IsA $msg msg] } {
	return -code error "$msg is not a valid message"
    }
    upvar \#0 $msg M

    # Create a new message within proper factory
    set dup [NewMessage $factory]
    upvar \#0 $dup D

    # Copy everything, which could overwrite factory information
    array set D [array get M]

    # Fix factory information
    if { $factory ne "" } {
	set D(factory) $factory
    }

    return $dup
}


proc ::stomp::message::new { cmd { factory "" } } {
    variable STOMP

    # Check if command is a known STOMP command.  Be cool about casing
    # and finding first matching command.
    set commands [concat $STOMP(client) $STOMP(server)]
    set idx [lsearch -glob $commands [string toupper $cmd]]
    if { $idx < 0 } {
	return -code error "$cmd is not a valid and known STOMP command"
    }

    set msg [NewMessage $factory]
    upvar \#0 $msg M

    set M(command) [lindex $commands $idx]
    set M(factory) $factory;  # Factory for message generation
    set M(headers) {};        # Headers for message
    set M(body) "";           # Body of message

    # Return identifier of message.
    return $msg
}


proc ::stomp::message::factory { sock args } {
    variable STOMP

    set id [Identifier]
    set factory [namespace current]::factory_$id
    upvar \#0 $factory FCTRY

    # Get options from arguments
    foreach opt [array names STOMP -*] {
	GetOpt args $opt FCTRY($opt) $STOMP($opt)
    }

    # Initialise internal factory data
    set FCTRY(sock)  $sock;     # Socket used by the factory.
    set FCTRY(id) $id;          # Identifier of factory.

    return $factory
}


package provide stomp::message $::stomp::message::version
