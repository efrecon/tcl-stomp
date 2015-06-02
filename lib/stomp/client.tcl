package require Tcl
package require md5

package require stomp
package require stomp::message

namespace eval ::stomp::client {
    variable STOMP
    if {![info exists STOMP] } {
	array set STOMP {
	    -host          localhost
	    -port          61613
	    -user          ""
	    -password      ""
	    -vhost         ""
	    -heartbeat-in  10000
	    -heartbeat-out 100
	    -forgiveness   1.5
	    -reconnect     1000
	    -liveness      ""
	    -socketCmd     {socket}
	    -removal       100
	    receiptHeader  "receipt"
	    types          {client subscription}
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]

	# Bridge some local procedures from main namespace into here
	# so we can use them without having to export them.
	foreach cmd [list Debug Identifier DecideBeat GetOpt] {
	    namespace eval [namespace current] \
		[string map [list @cmd@ $cmd] {
		    proc @cmd@ { args } {
			namespace eval [namespace parent] @cmd@ $args
		    }
		}]
	}
    }
}



# IMPLEMENTATION NOTES:
#
# Sending of messages should always be done via the Send command,
# since this command will detect when message were not sent
# (connection lost) and when the connection should be reopened (if
# applicable and requested for from the arguments).
#

# ::stomp::client::Send -- Reconnecting wrapper around send
#
#       Send a message to a remote server, making sure we trigger a
#       reconnection to the server if necessary.
#
# Arguments:
#	client	Identifier of client context
#	msg	Identifier of message to be sent
#	reconnect	Should we attempt to reconnect if connection lost.
#	keep	Keep (or throw away, default) message once sent.
#
# Results:
#       1 if the message was sent, 0 otherwise
#
# Side Effects:
#       None.
proc ::stomp::client::Send { client msg { reconnect 1 } { keep 0 } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Send message and throw it away if requested to
    set sent [[namespace parent]::message::send $msg]
    if { [string is false $keep] } {
	[namespace parent]::message::delete $msg
    }

    # If sending of message failed, meaning the connection to remote
    # server has been lost, mediate library caller and disconnect
    # instantaneously from server.  Schedule reconnection if
    # applicable and if requested to.
    if { [string is false $sent] } {
	if { $reconnect } {
	    Debug 3 "Connection to server at $CLT(-host):$CLT(-port) lost,\
                     will reconnect in a while if applicable"
	} else {
	    Debug 3 "Connection to server at $CLT(-host):$CLT(-port) lost,\
                     forcing disconnection"
	}
	Liveness $client CLOSING
	Disconnect $client $reconnect;
    }

    return $sent
}


# ::stomp::client::Liveness -- Mediate connection state to callers
#
#       Perform callbacks to callers to inform them about the
#       connection state, if a handler for reception of this
#       information was given at creation time.  The possible states
#       are:
#       CONNECTING    Library is currently connecting to server.
#       CONNECTED     Library is connected to server, handshake successfull.
#       DISCONNECTING Currently disconnecting from server
#       DISCONNECTED  Library disconnected.
#       CLOSING       Currently closing (abruptly) connection, lost conn.
#
# Arguments:
#	client	Identifier of client context
#	state	State to send back to callers.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::client::Liveness { client { state "" } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    if { $state ne "" && $CLT(-liveness) ne "" } {
	Debug 4 "Mediating new current state to callers: $state"
	if { [catch {eval [linsert $CLT(-liveness) end \
			       $client $state]} res]} {
	    Debug 3 "Error when calling liveness callback: $res"
	}
    }
}


# ::stomp::client::Disconnect -- Physical disconnection
#
#       Perform a physical disconnection from the server, emptying all
#       states that are relevant to the current connection.
#
# Arguments:
#	client	Identifier of client context
#
# Results:
#       None.
#
# Side Effects:
#       Closes socket connection to remote end.
proc ::stomp::client::Disconnect { client { reopen 0 } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    Debug 2 "Closing connection to $CLT(-host):$CLT(-port)"

    # Remove possible heart-beat and keep-alive contexts for that
    # socket.
    [namespace parent]::message::keepAlive $CLT(sock) -1
    [namespace parent]::message::heartBeat $CLT(sock) -1
 
    if { $CLT(reader) ne "" } {
	[namespace parent]::message::delete $CLT(reader)
    }
    if { $CLT(factory) ne "" } {
	[namespace parent]::message::delete $CLT(factory)
    }

    # Clean up all states and physically close socket to server.
    set CLT(reader) ""
    set CLT(factory) ""
    catch {close $CLT(sock)}
    set CLT(sock) ""
    set CLT(isConnected) 0

    # Tell callers
    Liveness $client DISCONNECTED

    # Reconnect if we should, i.e. it is applicable according to
    # current context, thus according to where we are called from, but
    # it also is applicable according to parameters to the library,
    # i.e. -reconnect time in milliseconds.
    if { [string is true $reopen] && $CLT(-reconnect) >= 0 \
	     && $CLT(reconnect) eq "" } {
	Debug 2 "Reconnecting to $CLT(-host):$CLT(-port)\
                 in $CLT(-reconnect) ms."
	set CLT(reconnect) \
	    [after $CLT(-reconnect) [namespace current]::Connect $client]
    }
}


proc ::stomp::client::SendError { client { errMsg "" } } {
    variable STOMP

    upvar \#0 $client CLT

    set msg [[namespace parent]::message::new ERROR $CLT(factory)]
    if { $errMsg ne "" } {
	# Even if adding the body does not work, we send the error
	# anyway. In other words, ignore return value from setBody...
	[namespace parent]::message::setBody $msg $errMsg
    }
    Send $client $msg

    # Do we attempt to reconnect.  What do we do with suscriptions,
    # should we re-tune them if we attempt to re-connect, once we have
    # the new socket?
    Disconnect $client 1
}


# ::stomp::client::ForceClose -- Force disconnection
#
#       Send an error message to the server, telling it that we've
#       missed him very much (no heart-beat, even after the grace
#       period) and close forcefully the socket connection to the
#       server.
#
# Arguments:
#	client	Identifier of client context
#	hb	Identifier of the heart beat context.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::client::ForceClose { client { hb "" } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Telling callers we are closing by hand...
    Liveness $client CLOSING

    # Tell remote end that we are now giving up...
    SendError $client "Stale connection: Missed heart-beat!"
}


proc ::stomp::client::ReaderError { client type { msg "" } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    Debug 3 "Error when reading from server: $msg"

    # Telling callers we are closing by hand...
    Liveness $client CLOSING

    # Do we attempt to reconnect.  What do we do with suscriptions,
    # should we re-tune them if we attempt to re-connect, once we have
    # the new socket?
    Disconnect $client 1
}


# ::stomp::client::HandleMessage -- Dispatch incoming message
#
#       Dispatch incoming message coming from server to handler that
#       was registered at creation. Handle ACKnowledgments in
#       cooperation with the handler that is being called at message
#       reception.  This depends on the ack-mode of the STOMP
#       subscription. If the mode is auto, nothing is sent back. If
#       the mode is client-individual, every message is acknowledged
#       or not, based on the boolean returned by the handler.  If the
#       mode is client, all previous messages for that subscription
#       are acknowledged whenever the handler returns a positive
#       boolean.
#
# Arguments:
#	sub	Identifier of the subscription, as created by subscribe
#	msg	Message to be passed further to handler
#
# Results:
#       None.
#
# Side Effects:
#       Send back ACK or NACK messages depending on interaction with
#       handler and current ACK-mode for subscription.
proc ::stomp::client::HandleMessage { sub msg } {
    variable STOMP

    upvar \#0 $sub SUB
    upvar \#0 $SUB(client) CLT

    # Initialise what kind of message response we should send back to
    # the server, default is not to send any ACK or NACK.
    set acker "";

    # Call handler and understand returned boolean as ACKing value,
    # whenever relevant.
    Debug 4 "Passing message further to subscription handler"
    if { [catch {eval [linsert $SUB(-handler) end $msg]} res]} {
	Debug 3 "Error when callbacking subscription handler: $res"
	if { $SUB(-ack) eq "client-individual" } {
	    set acker NACK
	}
    } else {
	switch $SUB(-ack) {
	    client-individual {
		set acker [expr {[string is true $res] ? ACK : NACK}]
	    }
	    client {
		# ACK bursts of messages, so send back an
		# ACK only when handler has responded yes.
		if { [string is true $res] } {
		    set acker ACK
		}
	    }
	}
    }

    # If we should send back a response, do this now.
    if { $acker ne "" } {
	set response [[namespace parent]::message::new $acker $CLT(factory)]
	[namespace parent]::message::setHeader $response \
	    id $SUB(-identifier)
	Send $SUB(client) $response
    }
}


# ::stomp::client::HandleReceipt -- Receipt handler
#
#       Detect if this message is a receipt and callback the handler
#       that matches the receipt identifier, if any.
#
# Arguments:
#	client	Identifier of client context
#	msg	Parsed message, as passed from message::reader machinery
#
# Results:
#       0 if not a receipt, 1 if there was a receipt but nothing to
#       handle it, 2 if we were waiting for the receipt, 3 if we
#       handled the receipt properly, -1 if the handler failed taking
#       care of the receipt properly (error catched).
#
# Side Effects:
#       None.
proc ::stomp::client::HandleReceipt { client msg } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Get identifier for receipt from message
    set result 0
    set receiptId [[namespace parent]::message::getHeader $msg receipt-id]
    
    # Find command to callback and do the callback, if any
    if { $receiptId eq "" } {
	Debug 2 "No receipt identifier provided by server"
    } else {
	set result 1;               # Message had a receipt identifier
	set receipts {};   # New receipt list, to be
	# reconstructed without the one
	# we've just received.
	foreach {rId cbCmd} $CLT(receipts) {
	    if { $rId eq $receiptId } {
		set result 2;       # We had  a matching receipt waiting
		Debug 4 "Passing incoming receipt $rId to callback"
		if { [catch {eval [linsert $cbCmd end \
				       $client $rId]} res]} {
		    Debug 3 "Error when calling receipt callback: $res"
		    set result -1;  # Callback, but failure
		} else {
		    set result 3;   # Callback and success.
		}
	    } else {
		lappend receipts $rid $cbCmd
	    }
	}
	set CLT(receipts) $receipts
    }

    return $result
}



# ::stomp::client::Dispatch -- Intercept interesting commands and dispatch
#
#       Receive commands from remote ends, intercept those of interest
#       in order to entertain the context of connections and dispatch
#       to handlers of interest.
#
# Arguments:
#	client	Identifier of client context
#	msg	Parsed message, as passed from message::reader machinery
#
# Results:
#       None.
#
# Side Effects:
#       Call registered handlers of various sorts, responds to remote
#       ends through additional messages and close connection if
#       necessary.
proc ::stomp::client::Dispatch { client msg } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Fetch data from incoming message and perform debug dump if
    # appropriate.
    set cmd [[namespace parent]::message::getCommand $msg]
    array set HDR [[namespace parent]::message::getHeaders $msg]
    Debug 4 "Receive $cmd"
    foreach k [array names HDR] {
	Debug 5 "\t${k}:$HDR($k)"
    }

    # Treat incoming command
    switch $cmd {
	"CONNECTED" {
	    set CLT(isConnected) 1

	    # Decide Heartbeats to and from remote end.
	    set srvhb_in 0
	    set srvhb_out 0
	    if { [array names HDR heart-beat] ne "" } {
		foreach {srvhb_out srvhb_in} [split $HDR(heart-beat) ","] \
		    break
	    }
	    set CLT(hb-out) [DecideBeat $CLT(-heartbeat-out) $srvhb_in]
	    set CLT(hb-in) [DecideBeat $CLT(-heartbeat-in) $srvhb_out]

	    # Ensure that we will be sending some data to the remote
	    # end at least every hb-out milliseconds.
	    if { $CLT(hb-out) > 0 } {
		Debug 3 "Heart-beat to server is $CLT(hb-out) ms."
		[namespace parent]::message::keepAlive \
		    $CLT(sock) $CLT(hb-out)
	    } else {
		Debug 3 "No heart-beat to server"
	    }

	    # Create a heart-beat reception context of slightly more
	    # than hb-in milliseconds (factored by the forgiveness
	    # parameter), arranging to close the connection after that
	    # relax .
	    if { $CLT(hb-in) > 0 } {
		Debug 3 "Heart-beat from server is $CLT(hb-in) ms."
		set expiry [expr int($CLT(-forgiveness)*$CLT(hb-in))]
		[namespace parent]::message::heartBeat $CLT(sock) $expiry \
		    [list [namespace current]::ForceClose $client]
	    } else {
		Debug 3 "No heart-beat from server"
	    }

	    # Tell callers we've successfully connected to server.
	    Liveness $client CONNECTED
	}
	"ERROR" {
	    set bdy [[namespace parent]::message::getBody $msg]
	    if { $bdy ne "" } {
		Debug 2 "Received server error: $bdy"
	    }
	    Liveness $client ERROR
	    # Close connection and mark client as disconnected
	    Disconnect $client 1
	}
	"MESSAGE" {
	    # Find relevant subscription, if possible and dispatch to
	    # handler for that subscription.  Finding no subscription
	    # is not an error since we allow callers to pass through
	    # our subscription mechanisms without using internal
	    # subscription mechanisms.
	    set sub [[namespace parent]::message::getHeader $msg subscription]
	    if { $sub eq "" } {
		Debug 2 "No subscription identifier passed by server"
	    } else {
		# Look for subscription owning the message, if possible.
		set allsubs \
		    [info vars [namespace current]::subscription_$CLT(id)_*]
		foreach subscription $allsubs {
		    upvar \#0 $subscription SUB
		    if { $SUB(-identifier) eq $sub && $SUB(active) } {
			HandleMessage $subscription $msg
			break;   # Done, we can only have one...
		    }
		}
	    }
	}
	"RECEIPT" {
	    # Take care of receipt, if relevant.
	    HandleReceipt $client $msg
	}
    }

    # Callback existing command handlers, if any, now that we've cooked
    # internal cuisine...
    foreach {filter handler} $CLT(handlers) {
	if { [string match -nocase $filter $cmd] } {
	    Debug 4 "Passing incoming STOMP command $cmd further to\
                     handler $handler"
	    if { [catch {eval [linsert $handler end $cmd]} res] } {
		Debug 3 "Error when callbacking STOMP command dispatcher:\
                         $res"
	    }
	}
    }
}


# ::stom::Connect -- Connect to remote server and initiate connection
#
#       Connect to remote server as specified at client context
#       creation and initialise connection by sending a CONNECT
#       command.  Arrange for receiving messages coming back from
#       server in ::stomp::client::Dispatch for connection bookkeeping.
#
# Arguments:
#	client	Identifier of connection, as returned by stomp::connect
#
# Results:
#       None.
#
# Side Effects:
#       Open a socket connection to remote server and arrange for
#       incoming messages to be received, parsed and passed further to
#       registered handlers.
proc ::stomp::client::Connect { client } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Tell callers that we are trying to connect to remote end.
    Liveness $client CONNECTING

    # Open socket connection to client and tune socket paramaters
    if { $CLT(factory) ne "" } {
	[namespace current]::message::delete $CLT(factory)
	set CLT(factory) ""
    }
    set sockCmd $CLT(-socketCmd)
    if { $sockCmd eq "" } { set sockCmd [list socket] }
    set CLT(sock) ""
    set CLT(reconnect) ""
    if { [catch {eval [linsert $sockCmd end $CLT(-host) $CLT(-port)]} sock] \
	     && $CLT(reconnect) eq ""} {
	Debug 2 "Cannot connect to $CLT(-host):$CLT(-port)\
                 retrying in $CLT(-reconnect) ms."
	set CLT(reconnect) \
	    [after $CLT(-reconnect) [namespace current]::Connect $client]
	return ;
    } else {
	set CLT(sock) $sock
    }
    fconfigure $CLT(sock) \
	-blocking 0 \
	-buffering full \
	-translation {auto lf} \
	-encoding utf-8

    # Create factory to generate messages from
    set CLT(factory) [[namespace parent]::message::factory $CLT(sock) \
			  -type client]

    # Create connection message, do not use STOMP so as to be
    # compatible with older versions of the protocol.
    set msg [[namespace parent]::message::new CONNECT $CLT(factory)]

    # Add user/password information if we had specified a username
    if { $CLT(-user) ne "" } {
	[namespace parent]::message::setHeader $msg \
	    login $CLT(-user) \
	    passcode $CLT(-password)
    }

    # Handle virtual/regular host
    if { $CLT(-vhost) ne "" } {
	[namespace parent]::message::setHeader $msg host $CLT(-vhost)
    } else {
	[namespace parent]::message::setHeader $msg host $CLT(-host)
    }

    # Heartbeat specification
    [namespace parent]::message::setHeader $msg heart-beat \
	[join [list $CLT(-heartbeat-out) $CLT(-heartbeat-in)] ","]

    # Announce the versions that we support, i.e. all versions at
    # present.
    upvar \#0 [namespace parent]::STOMP SUPER
    [namespace parent]::message::setHeader $msg accept-version \
	[join $SUPER(versions) ","]

    # Send CONNECTion message and delete it once sent.
    Send $client $msg

    # Register internal handler for message reception
    set CLT(reader) [[namespace parent]::message::reader $CLT(factory) \
			 [list [namespace current]::Dispatch $client] \
			 [list [namespace current]::ReaderError $client]]
}


proc ::stomp::client::IsA { varname type } {
    variable STOMP

    set idx [lsearch -glob $STOMP(types) $type]
    if { $idx < 0 } {
	return -code error "$type is not a knonw STOMP\
                            [join $STOMP(types) "or "]"
    }
    set type [lindex $STOMP(types) $idx]
    if { [lsearch [info vars [namespace current]::${type}_*] $varname]<0 } {
	return 0
    }
    return 1
}


# ::stomp::client::handler -- Register command handler
#
#       Register a command that will be called every time a message
#       matching the filter will arrive from the remote end.  Whenever
#       a message that matches arrives, the command will be called
#       with the identifie of the message added to its parameter list.
#
# Arguments:
#	client	Identifier of client, as returned by connect
#	cbCmd	Command to call.
#	filter	glob-matching filter for message command type.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::client::handler { client cbCmd { filter * } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    lappend CLT(handlers) $filter $cbCmd
}



# ::stomp::client::AddReceipt -- Add receipt request
#
#       Modify a message so that it will contain a receipt request and
#       arranges for a command to be called on the reception of the
#       receipt.
#
# Arguments:
#	client	Identifier of client, as returned by connect
#	msg	Message to modify header for receipt request
#	rCmd	Command to call on receipt.
#
# Results:
#       Return the identifier of the receipt request within the header.
#
# Side Effects:
#       Will ask the server for a receipt, which will be returned as
#       soon as the server has processed the message, and will result
#       in the command passed as a parameter to be called back at that
#       time.
proc ::stomp::client::AddReceipt { client msg { rCmd "" } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    set receiptId [join [list $STOMP(receiptHeader) [Identifier]] "-"]
    [namespace parent]::message::setHeader $msg "receipt" $receiptId
    Debug 4 "Requesting message receipt with identifier $receiptId"
    if { $rCmd ne "" } {
	# Remember unique receipt identifier together with
	# callback command to call on receipt reception.
	lappend CLT(receipts) $receiptId $rCmd
    }

    return $receiptId
}


# ::stomp::client::send -- Send message to remote end.
#
#       Send message to remote end, this is mainly a wrapper around
#       the [namespace parent]::message::send command, though providing a more
#       programmer-friendly API.  In addition to the mandatory
#       destination (typically a queue path), the command takes a
#       number of dash-led options followed by their values. These are
#       -headers      List of key and values for additional headers
#       -type         Content type of the message.
#       -content-type Same as above, have precedence.
#       -body         Body content of message
#
# Arguments:
#	client	Identifier of client, as returned by connect
#	dest	Destination for message
#	args	List of dash-led options and their values, see above.
#
# Results:
#       1 if the message could be sent, 0 otherwise
#
# Side Effects:
#       Send message along socket to remote end.
proc ::stomp::client::send { client dest {args {}}} {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # Only send if we actually are in connected state.
    if { $CLT(isConnected) } {
	# Get options from the remaining of the procedure arguments,
	# taking some decent defaults.
 	GetOpt args -headers headers {}
 	GetOpt args -type type ""
	# Precedence, overrides -type
 	GetOpt args -content-type type $type
 	GetOpt args -body body ""
	GetOpt args -receipt rcv {};   # cb for receipt

	# Constuct a new message, pushing out all required and
	# optional headers
	set msg [[namespace parent]::message::new SEND $CLT(factory)]
	[namespace parent]::message::setHeader $msg destination $dest
	if { $type ne "" } {
	    [namespace parent]::message::setHeader $msg content-type $type
	}
	foreach {hdr val} $headers {
	    [namespace parent]::message::setHeader $msg $hdr $val
	}
	# If we've asked a command to be called once message is
	# processed at server, generate a receipt identifier and place
	# the command in receipt queue.
	if { $rcv ne "" } {
	    AddReceipt $client $msg $rcv
	}

	# Add body to message, if any, then send and discard
	if { $body ne "" } {
	    if { ![[namespace parent]::message::setBody $msg $body] } {
		Debug 2 "Message too long"
		[namespace parent]::message::delete $msg
		return 0
	    }
	}
	return [Send $client $msg]
    }

    return 0
}


# ::stomp::client::subscribe -- Subscribe to messages
#
#       Subscribe to messages for a given topic/destination at server
#       and possibly arranges for subscription callbacks to occur on
#       message reception.  Apart from the mandatory destination
#       topic, this command takes a number of optional dash-led
#       options and their values.  These are:
#       -handler    Command to callback with message identifier appended
#       -headers    List of key and values specifying additional headers
#       -ack        Ack mode: auto, client or client-individual
#       -identifier Identifier of subscription.
#
# Arguments:
#	client	Identifier of client, as returned by connect
#	dest	Destination for messages to receive from.
#	args	List of dash-led options and their values, see above.
#
# Results:
#       Return either the identifier of the subscription (if an
#       handler is provided) or the network identifier of the
#       subscription on the wire, if no handler is provided.  An empty
#       string is returned on errors.
#
# Side Effects:
#       None.
proc ::stomp::client::subscribe { client dest {args {}}} {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # If connected create the subscription context and send a message
    # to server.
    if { $CLT(isConnected) } {
	# Get options from the remaining of the procedure arguments
 	GetOpt args -handler handlerCmd ""
	GetOpt args -headers headers {}
 	GetOpt args -ack ack "auto"
 	GetOpt args -identifier subId "";    # Own id
	GetOpt args -receipt rcv "";         # receipt cb

	# Check ack mode, arrange at least to allow for casing mistakes.
	set ack [string tolower $ack]
	set modes [list auto client client-individual]
	if { [lsearch [list "auto" "client" "client-individual"] $ack]<0 } {
	    return -code error \
		"$ack not a known mode, should be one of [join $modes ", "]"
	}

	set id [Identifier];  # Generate a numerical id for sub.
	if { $handlerCmd ne "" } {
	    # We have a handler command, meaning we want the stomp
	    # module to help us dealing with subscription and
	    # messages: Create a context for the subscription!
	    set subscription [namespace current]::subscription_$CLT(id)_$id
	    upvar \#0 $subscription SUB
	    
	    # Use namespace variable name as an identifier for the
	    # subscription if none forced from the outside.
	    if { $subId eq "" } {
		set subId [string map [list _ -] \
			       [namespace tail $subscription]]
	    }

	    # Remember subscription details.
	    set SUB(id) $id;                # Identifier of subscription
	    set SUB(client) $client;        # Parent client hosting sub.
	    set SUB(-identifier) $subId;    # Identifier of sub on wire
	    set SUB(-destination) $dest;    # Destination STOMP queue
	    set SUB(-handler) $handlerCmd;  # Command to call on msg arrival
	    set SUB(-ack) $ack;             # Message reception ACK mode
	    set SUB(active) 1;              # Is subscription active?
	} elseif { $subId eq "" } {
	    # Generate an identifier for the subscription. Make sure
	    # this isn't the same as for the subscriptions that we
	    # want to support internally to ease lookup on message
	    # reception.
	    set subId selfsub_$CLT(id)_$id
	}
	
	# Send subscription message to remote server, this will occur
	# in all cases, including we did not specify a handler
	# commands. In other words, callers are free to handle
	# subscriptions themselves by just subscribing using this
	# procedure and not providing any handler at all.
	set msg [[namespace parent]::message::new SUBSCRIBE $CLT(factory)]
	[namespace parent]::message::setHeader $msg \
	    id $subId \
	    destination $dest \
	    ack $ack
	# If we've asked a command to be called once message is
	# processed at server, generate a receipt identifier and place
	# the command in receipt queue.
	if { $rcv ne "" } {
	    AddReceipt $client $msg $rcv
	}
	foreach {hdr val} $headers {
	    [namespace parent]::message::setHeader $msg $hdr $val
	}
	Send $client $msg

	if { $handlerCmd ne "" } {
	    return $subscription;   # Return identifier of sub in Tcl
	} else {
	    return $subId;          # Return identifier of sub on wire.
	}
    } else {
	Debug 2 "Client $client to $CLT(-host):$CLT(-port) not connected\
                 (yet)"
    }

    return "";   # Catch all for errors.
}


proc ::stomp::client::RemoveSub { sub } {
    variable STOMP

    if { ![IsA $sub subscription] } {
	return -code error "$sub is not a known subscription"
    }
    unset $sub
}


proc ::stomp::client::unsubscribe { sub } {
    variable STOMP

    if { ![IsA $sub subscription] } {
	return -code error "$sub is not a known subscription"
    }
    
    upvar \#0 $sub SUB
    upvar \#0 $SUB(client) CLT
    
    if { $CLT(isConnected) } {
	set msg [[namespace parent]::message::new UNSUBSCRIBE $CLT(factory)]
	[namespace parent]::message::setHeader $msg id $SUB(-identifier)
	Send $SUB(client) $msg
    }

    # Inactivate subscription and schedule its removal shortly.
    set SUB(active) 0
    if { $CLT(-removal) >= 0 } {
	after $CLT(-removal) [list [namespace current]::RemoveSub $sub]
    } else {
	after idle [list [namespace current]::RemoveSub $sub]
    }
}


proc ::stomp::client::subscriptions { client { filter "*" } } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    set subscriptions {}
    foreach s [info vars [namespace current]::subscription_$CLT(id)_*] {
	upvar \#0 $s SUB
	if { [string match $filter $SUB(-destination)] && $SUB(active) } {
	    lappend subscriptions $s
	}
    }
    return $subscriptions
}


# ::stomp::client::HandleDisconnect -- Perform disconnection
#
#       Performs physical disconnection by closing server and cleaning
#       up all state.
#
# Arguments:
#	client	Identifier of client, as returned by connect
#	rId	Identifier of receipt
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::client::HandleDisconnect { client rId } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    Debug 2 "Server has gracefully acknowledged disconnection, closing down"
    Disconnect $client
}


# ::stomp::client::disconnect -- Disconnect gracefully from server
#
#       Disconnect from server in a gracefull manner, i.e. by sending
#       a DISCONNECT message and gently waiting for server
#       disconnection ack.
#
# Arguments:
#	client	Identifier of client, as returned by connect
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::client::disconnect { client } {
    variable STOMP

    if { ![IsA $client client] } {
	return -code error "$client is not a known STOMP client connection"
    }
    upvar \#0 $client CLT

    # If we are connected, tell the server that we want to disconnect
    # and gently wait for the disconnection acknowledgment.  Make sure
    # we don't reopen the connection once we've requested for (final)
    # disconnection.
    if { $CLT(isConnected) } {
	Liveness $client DISCONNECTING
	set msg [[namespace parent]::message::new DISCONNECT $CLT(factory)]
	AddReceipt $client $msg [namespace current]::HandleDisconnect
	Send $client $msg 0;  # Send but don't attempt to
			      # automatically reconnect since we've
			      # explicitely asked to disconnect.
    } else {
	# Cancel reconnection attempts by now, since we are
	# disconnected and we've explicitely requested to disconnect.
	if { $CLT(reconnect) ne "" } {
	    after cancel $CLT(reconnect)
	    set CLT(reconnect) ""
	}
    }
}


# ::stomp::client::connect -- Create client connection
#
#       Open connection to remote STOMP server and make sure that it
#       persists over time.  The procedure accept a number of optional
#       dash-led options and their values, these are:
#       -host          Hostname of remote server
#       -port          Port number
#       -user          Username for authentication
#       -password      Password for authentication
#       -vhost         Virtual host at remote server
#       -heartbeat-in  Incoming heartbeat
#       -heartbeat-out Outgoing heartbeat.
#       -reconnect     Milliseconds between reconnections, neg. to turn off.
#       -liveness      Command to be called with connection state information
#
# Arguments:
#	args	List of dash-led options and their values, see above.
#
# Results:
#       Return an identifier for the connection.
#
# Side Effects:
#       Open STOMP connection to server and keep it active.
proc ::stomp::client::connect { args } {
    variable STOMP
    
    set id [Identifier]
    set client [namespace current]::client_$id
    upvar \#0 $client CLT

    # Get options from arguments.
    foreach opt [array names STOMP -*] {
	GetOpt args $opt CLT($opt) $STOMP($opt)
    }
    
    # Initialise internal connection data.
    set CLT(isConnected) 0;   # Is client in connected state at server?
    set CLT(sock) "";         # Socket to server 
    set CLT(id) $id;          # Identifier of client (for sub-objects)
    set CLT(reader) "";       # Identifier of reader for incoming messages
    set CLT(handlers) "";     # List of <filter,handler> for incoming
			      # messages dispatch.
    set CLT(receipts) {};     # List of receipts and associated callbacks.
    set CLT(reconnect) "";    # Identifier of reconnection attempt
    set CLT(factory) "";      # Message factory
    
    Connect $client

    return $client
}

package provide stomp::client $::stomp::client::version
