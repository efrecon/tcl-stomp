package require Tcl

package require stomp
package require stomp::message

# IMPLEMENTATION NOTES and TODO
#
# By design, each type of incoming message is treated in separate
# procedures (in order to relieve the length of the "big-switch",
# errors will be sent directly from these procedures, but response to
# the client will be sent from the main switch in Dispatch.  This
# makes the inner workings of the protocol more visible.
#
# There is no semantic in the destination, retaining messages over
# time, etc.
#
# There is no handling of ack modes yet, meaning that we behave in
# "auto" mode, more or less.


namespace eval ::stomp::server {
    variable STOMP
    if {![info exists STOMP] } {
	array set STOMP {
	    -port          61613
	    -vhosts        ""
	    -allow         "*"
	    -deny          ""
	    -heartbeat-in  10000
	    -heartbeat-out 100
	    -users         "admin:password"
	    -forgiveness   3.5
	    -socketCmd     {socket}
	    types          {server client vhost destination subscription}
	    notAUser       "-=-=-=-=----=-=-=-=-"
	    serverName     "TclSTOMP/%version%"
	    default        localhost
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
    namespace path [linsert [namespace path] 0 [namespace parent]]
}

# ::stomp::server::IsA -- Detect if variable is our object
#
#       Check that a variable name is of one of our "object" types and
#       that it exsts.
#
# Arguments:
#	varname	Name of variable within this namespace
#	type	Type to test against
#
# Results:
#       0 if not, 1 if free to go forward.
#
# Side Effects:
#       None.
proc ::stomp::server::IsA { varname type } {
    variable STOMP

    set idx [lsearch -glob $STOMP(types) $type]
    if { $idx < 0 } {
	return -code error "$type is not a knonw STOMP\
                            [join $STOMP(types) " or "]"
    }
    set type [lindex $STOMP(types) $idx]
    if { [lsearch [info vars [namespace current]::${type}_*] $varname]<0 } {
	return 0
    }
    return 1
}


# ::stomp::server::Disconnect -- Disconnect from an existing client
#
#       Disconnects from the remote client and remove all state for
#       the client connection.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#
# Results:
#       None.
#
# Side Effects:
#       Close socket and discard all client-related information
proc ::stomp::server::Disconnect { c } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV
    upvar \#0 $CLT(vhost) VHOST

    Debug 2 "Closing connection to $CLT(-addr):$CLT(-port)"

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

    # Remove all subscriptions that this client had, meaning we can
    # perhaps cleanup a number of existing destinations.
    set allsubs [info vars \
		     [namespace current]::subscription_$SRV(id)_$VHOST(id)_*]
    foreach s $allsubs {
	upvar \#0 $s SUB
	if { $SUB(client) eq $c } {
	    unset $s
	}
    }
    DestinationsCleanup $CLT(vhost)

    catch {close $CLT(sock)}
    unset $c
}


# ::stomp::server::Send -- Send a message to client
#
#       Sends a message to a remote client, this procedure is a
#       wrapper around the message::send command, providing automatic
#       discovery of disconnected clients and message book-keeping.
#       The message will be sent on the socket that is registered to
#       the socket factory associated to the client, unless a specific
#       socket is provided in the arguments.
#
# Arguments:
#	c	Identifier of remote client.
#	msg	Message to be delivered
#	keep	Should we keep the message, default is to delete
#	sock	Alternative socket to send message on.
#
# Results:
#       1 if the message was sent and received, 0 otherwise.
#
# Side Effects:
#       None.
proc ::stomp::server::Send { c msg { keep 0 } { sock "" } } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT

    # Send message and throw it away if requested to
    set sent [[namespace parent]::message::send $msg $sock]
    if { [string is false $keep] } {
	[namespace parent]::message::delete $msg
    }

    # If sending of message failed, meaning the connection to remote
    # server has been lost, mediate library caller and disconnect
    # instantaneously from server.  Schedule reconnection if
    # applicable and if requested to.
    if { [string is false $sent] } {
	Debug 3 "Connection from client at $CLT(-addr):$CLT(-port) lost,\
                 forcing disconnection"
	Disconnect $c
    }
    
    return $sent
}


# ::stomp::server::Authorised -- Check authorisation
#
#       Check if the provided username and passwords are authorised to
#       access the server.  Authorisation credentials are kept in the
#       -users option of the servers, and are composed of a list of
#       pairs, in which the login and password are kept separated by a
#       colon sign.  Note that this procedure does not perform *all*
#       of the authorisation mechanism, it only checks when a login
#       name has been provided.
#
# Arguments:
#	s	Identifier of server, as returned by new
#	login	Login name sent by client.
#	passwd	Password for user.
#
# Results:
#       1 if authorised, 0 in all other cases.
#
# Side Effects:
#       None.
proc ::stomp::server::Authorised { s login passwd } {
    variable STOMP

    upvar \#0 $s SRV

    foreach auth $SRV(-users) {
	foreach {uname pwd} [split $auth ":"] break
	if { $uname eq $login && $pwd eq $passwd } {
	    return 1
	}
    }
    return 0
}
    

# ::stomp::server::SendError -- Send error to client and disconnect
#
#       Send an error message to the remote client and forcedly close
#       the connection.
#
# Arguments:
#	c	Identifier of client, as created during Accept
#	errMsg	Error message, empty for none.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::stomp::server::SendError { c { errMsg "" } } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT

    # Create ERROR message, possibly with the textual message as a
    # body and send.
    set msg [[namespace parent]::message::new ERROR $CLT(factory)]
    if { $errMsg ne "" } {
	[namespace parent]::message::setBody $msg $errMsg
	Debug 3 "Sending error to client: $errMsg"
    }
    Send $c $msg

    # Disconnect whatever happens (Send might have disconnected and
    # remove client if problems occured).
    catch {Disconnect $c}
}


# ::stomp::server::ForceClose -- Force disconnection on missing heart-beat
#
#       This procedure is called back once no heart-beat has been
#       received from the remote client for the relaxed hand-shaked
#       agreed period. Send a gentle error message saying we are
#       missing the client and close the connection.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	hb	Identifier of heart-beat constuct, not used
#
# Results:
#       None.
#
# Side Effects:
#       Force disconnection
proc ::stomp::server::ForceClose { c { hb "" } } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT

    # Tell remote end that we are now giving up...
    SendError $c "Stale connection: Missed heart-beat!"
}


proc ::stomp::server::ReaderError { c type {msg ""} } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT

    Debug 4 "Error when reading from client $CLT(-addr):$CLT(-port): $msg"
    catch {Disconnect $c}
}


# ::stomp::server::HandleConnect -- Connect message handling
#
#       Parse a connection message from a remote client and update all
#       relevant internal state, sending erros whenever the connection
#       cannot be established.  This procedure will take care of
#       authorisation, virtual hosting and heart-beat handshaking.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	msg	CONNECT message to parse.
#
# Results:
#       Return the protocol version to use when talking to the client,
#       or a negative number (-1 actually).
#
# Side Effects:
#       None.
proc ::stomp::server::HandleConnect { c msg } {
    variable STOMP

    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV
    
    # Get and check login info, if relevant.  We use a username that
    # is highly impossible to detect the difference between no
    # username provided at all and empty username.
    set login [[namespace parent]::message::getHeader $msg login \
		   $STOMP(notAUser)]
    set passwd [[namespace parent]::message::getHeader $msg passcode]

    # Check authorisation
    if { $login eq $STOMP(notAUser) } {
	if { [llength $SRV(-users)] > 0 } {
	    SendError $c "Authorisation refused, no username provided"
	    return -1
	}
    } else {
	if { ![Authorised $CLT(server) $login $passwd] } {
	    SendError $c "Authorisation refused for $login"
	    return -1
	}
    }

    # Associate us to an existing virtual host, return an error if we
    # cannot find one.
    set CLT(vhost) ""
    set allhosts [info vars [namespace current]::vhost_$SRV(id)_*]
    set host [[namespace parent]::message::getHeader $msg host]
    if { $host eq "" } {
	set CLT(vhost) [lindex $allhosts 0]
    } else {
	foreach vh $allhosts {
	    upvar \#0 $vh VHOST
	    if { [string match -nocase $VHOST(-name) $host] } {
		set CLT(vhost) $vh
	    }
	}
    }
    if { $CLT(vhost) eq "" } {
	SendError $c "Virtual host $host does not exist here"
	return -1
    }

    # Decide upon which protocol version to use, this is the maximum
    # of the versions that we implement and the ones at the client.
    set versions [[namespace parent]::message::getHeader $msg \
		      accept-version "1.0"]
    upvar \#0 [namespace parent]::STOMP SUPER
    foreach v [split $versions ","] {
	if { [lsearch $SUPER(versions) $v] >= 0 } {
	    if { $CLT(version) < 0 } {
		set CLT(version) $v
	    } elseif { $v > $CLT(version) } {
		set CLT(version) $v
	    }
	}
    }
    
    # Decide upon which heartbeats to use for connection to client.
    set beats [[namespace parent]::message::getHeader $msg heart-beat "0,0"]
    foreach {clthb_out clthb_in} [split $beats ","] break
    set CLT(hb-out) [DecideBeat $SRV(-heartbeat-out) $clthb_in]
    set CLT(hb-in) [DecideBeat $SRV(-heartbeat-in) $clthb_out]

    # Ensure that we will be sending some data to the remote
    # end at least every hb-out milliseconds.
    if { $CLT(hb-out) > 0 } {
	Debug 3 "Heart-beat to client is $CLT(hb-out) ms."
	[namespace parent]::message::keepAlive $CLT(sock) $CLT(hb-out)
    } else {
	Debug 3 "No heart-beat to server"
    }
    
    # Create a heart-beat reception context of slightly more
    # than hb-in milliseconds (factored by the forgiveness
    # parameter), arranging to close the connection after that
    # relax .
    if { $CLT(hb-in) > 0 } {
	Debug 3 "Heart-beat from client is $CLT(hb-in) ms."
	set expiry [expr int($SRV(-forgiveness)*$CLT(hb-in))]
	[namespace parent]::message::heartBeat $CLT(sock) $expiry \
	    [list [namespace current]::ForceClose $c]
    } else {
	Debug 3 "No heart-beat from server"
    }

    # Return version to use when talking to client.
    return $CLT(version)
}


# ::stomp::server::Destination -- Create or return existing destination
#
#       Destinations are "paths" within a server, there will be a
#       destination construct for each path that has been discovered
#       during the protocol hand-shakes.  This procedure either
#       creates a destination context or return an existing one if the
#       path given as a parameter already was in a destination.
#
# Arguments:
#	vh	Identifier of the virtual host
#	dst	Path to the destination
#
# Results:
#       Return an identifier for the destination context.
#
# Side Effects:
#       None.
proc ::stomp::server::Destination { vh dst } {
    variable STOMP

    if { ![IsA $vh vhost] } {
	return -code error "$vh is not a known STOMP virtual host at server"
    }
    upvar \#0 $vh VHOST
    upvar \#0 $VHOST(server) SRV

    # Look among known destinations if we already have one that match
    # the path in the arguments.
    set alldests \
	[info vars [namespace current]::destination_$SRV(id)_$VHOST(id)_*]
    foreach d $alldests {
	upvar \#0 $d DST
	if { $DST(-destination) eq $dst } {
	    return $d
	}
    }

    # If none was found, dynamically create a new destination and
    # return it to caller.
    set id [Identifier]
    set d [namespace current]::destination_$SRV(id)_$VHOST(id)_$id
    upvar \#0 $d DST
    set DST(id) $id
    set DST(vhost) $vh
    set DST(server) $VHOST(server)
    set DST(-destination) $dst

    Debug 4 "Created new destination $dst"

    return $d
}

proc ::stomp::server::DestinationsCleanup { vh } {
    variable STOMP

    if { ![IsA $vh vhost] } {
	return -code error "$vh is not a known STOMP virtual host at server"
    }
    upvar \#0 $vh VHOST
    upvar \#0 $VHOST(server) SRV

    # Look among known destinations if we already have one that match
    # the path in the arguments.
    set alldests \
	[info vars [namespace current]::destination_$SRV(id)_$VHOST(id)_*]
    set allsubs \
	[info vars [namespace current]::subscription_$SRV(id)_$VHOST(id)_*]
    foreach d $alldests {
	upvar \#0 $d DST

	set orphane 1
	foreach s $allsubs {
	    upvar \#0 $s SUB
	    if { $SUB(-destination) eq $d } {
		set orphane 0
	    }
	}
	
	if { $orphane } {
	    Debug 4 "Removing destination $DST(-destination) from context,\
                     unused!"
	    unset $d
	}
    }
}


# ::stomp::server::SubscriptionAdd -- Register a client for subscription
#
#       Parse an incoming subscription message and arrange for the
#       client to receive messages that will be sent to that
#       destination in the future.  This procedure is aware of
#       versions prior to v 1.1 of the protocol, where subscriptions
#       could be handled without identifiers.  Errors are sent at
#       once.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	msg	SUBSCRIBE message to parse.
#
# Results:
#       Return the identifier of the subscription context created,
#       empty string on errors.
#
# Side Effects:
#       Creates an active subscription, meaning that the server will
#       send matching messages on that destination at a future time,
#       whenever messages are sent on that destination.
proc ::stomp::server::SubscriptionAdd { c msg } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV
    upvar \#0 $CLT(vhost) VHOST

    # Subscriptions MUST have a destination
    set destination [[namespace parent]::message::getHeader $msg destination]
    if { $destination eq "" } {
	SendError $c "No destination provided for subscription!"
	return ""
    }

    # Subscriptions MUST have a protocol identifier in most versions
    # of the specification, i.e. >= 1.1
    set protoId [[namespace parent]::message::getHeader $msg id]
    if { $protoId eq "" && $CLT(version) >= 1.1 } {
	SendError $c "No subscription identifier provided!"
	return ""
    }

    # Now generate a context for the subscription
    set id [Identifier]
    set s [namespace current]::subscription_$SRV(id)_$VHOST(id)_$id
    upvar \#0 $s SUB

    set SUB(id) $id
    set SUB(client) $c

    # Store protocol identifier of subscription, generate in rare
    # cases.  When we generate, we use the identifier of subscription
    # context, since it is made unique in time and space (or at least
    # pretty much so).
    set SUB(-id) $protoId
    if { $SUB(-id) eq "" } {
	set SUB(-id) [string map [list _ -] [namespace tail $s]]
    }

    # Store which is the destination for the message, this will
    # dynamically create the destination if necessary.
    set SUB(-destination) [Destination $CLT(vhost) $destination]

    # Store the acknowledment mode
    set SUB(-ack) [[namespace parent]::message::getHeader $msg ack "auto"]

    Debug 4 "Client at $CLT(-addr):$CLT(-port) listening to $destination"

    return $s
}


# ::stomp::server::SubscriptionRemove -- Unsubscribe
#
#       Parse an unsubscribe message and arrange for the client not to
#       receive messages sent on that destination anymore.  This
#       procedure is aware of versions of the protocol prior to 1.1,
#       meaning that it could remove SEVERAL matching subscriptions
#       based on the destination, if relevant.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	msg	UNSUBSCRIBE message to parse.
#
# Results:
#       Return the (list of) subscription(s) that were remove from the
#       context.
#
# Side Effects:
#       None.
proc ::stomp::server::SubscriptionRemove { c msg } {
    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV
    upvar \#0 $CLT(vhost) VHOST

    # Find what we should unsubscribe from, i.e. either the id or the
    # destination.  We prefer the identifier since this uniquely
    # identifies the subscription, using the name of the destination
    # could return several subscriptions.
    set protoId [[namespace parent]::message::getHeader $msg id]
    if { $protoId eq "" && $CLT(version) >= 1.1 } {
	SendError $c "No subscription identifier provided!"
	return ""
    }
    if { $protoId eq "" } {
	set destination \
	    [[namespace parent]::message::getHeader $msg destination]
    }

    # Remove the subscription(s) that match the id (preferred) or the
    # destination, that were specified in the message headers.
    set allsubs [info vars \
		     [namespace current]::subscription_$SRV(id)_$VHOST(id)_*]
    set removed {}
    foreach s $allsubs {
	upvar \#0 $s SUB
	
	if { $protoId eq "" } {
	    upvar \#0 $SUB(-destination) DST
	    if { $DST(-destination) eq $destination } {
		lappend removed $s
		unset $s
	    }
	} else {
	    if { $SUB(-id) eq $protoId } {
		lappend removed $s
		unset $s
	    }
	}
    }

    Debug 4 "Remove [llength $removed] matching subscription(s)."

    # Remove destinations that are unused.
    DestinationsCleanup $CLT(vhost)

    return $removed
}


# ::stomp::server::DeliverMessage -- Fan-out message
#
#       Fan-out an incoming message to all clients that have a
#       matching subscription for that destination.  The message will
#       be copied as many times as there are clients before being sent
#       further with the necessary additional header information.
#
# Arguments:
#	d	Identifier of destination, as created in Destination
#	msg	Message to fan out to all interested parties.
#
# Results:
#       Returns the list of receivers, i.e. the list of clients to
#       which the message was successfully sent.
#
# Side Effects:
#       None.
proc ::stomp::server::DeliverMessage { d msg } {
    variable STOMP

    if { ![IsA $d destination] } {
	return -code error "$d is not a known STOMP destination at server"
    }
    upvar \#0 $d DST
    upvar \#0 $DST(server) SRV
    upvar \#0 $DST(vhost) VHOST

    # Change message to be a MESSAGE!
    [namespace parent]::message::setCommand $msg MESSAGE

    # Send to all receivers that have expressed interest in that
    # message, i.e. that have a subscription matching that
    # destination.
    set receivers {}
    set allsubs [info vars \
		     [namespace current]::subscription_$SRV(id)_$VHOST(id)_*]
    foreach s $allsubs {
	upvar \#0 $s SUB
	upvar \#0 $SUB(client) CLT

	# If the destination of the subscription matches the
	# destination of the (incoming) message, duplicate the message
	# (via the factory), add header information and send it.  We
	# automatically add the identifier of the subscription and a
	# unique identifier for the message (incoming?).
	if { $SUB(-destination) eq $d } {
	    set dup [[namespace parent]::message::dup $msg $CLT(factory)]
	    [namespace parent]::message::setHeader $dup \
		subscription $SUB(-id) \
		message-id [string map [list _ -] [namespace tail $msg]]
	    # Add to list of receiver clients only if sending was
	    # successfull
	    if { [Send $SUB(client) $dup] } {
		lappend receivers $SUB(client)
	    }
	}
    }

    Debug 5 "Forwarded message to [llength $receivers] interested parties"

    # Return the list of receivers.
    return $receivers
}


# ::stomp::server::SendReceipt -- Send message receipt
#
#       Send a message receipt to a client if it was requested,
#       i.e. if there was a receipt-id in the headers of the message.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	msg	Message to possibly send receipt for.
#
# Results:
#       1 if receipt could be sent, 0 in all other cases.
#
# Side Effects:
#       None.
proc ::stomp::server::SendReceipt { c msg } {
    variable STOMP

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV

    # Look for receipt identifier in message header and send back
    # receipt if applicable.
    set receiptId [[namespace parent]::message::getHeader $msg receipt]
    if { $receiptId ne "" } {
	set receipt [[namespace parent]::message::new \
			 RECEIPT $CLT(factory)]
	[namespace parent]::message::setHeader $receipt \
	    receipt-id $receiptId
	return [Send $c $receipt]
    }

    return 0
}


# ::stomp::server::Dispatch -- Parse, analyse and treat incoming message
#
#       This is the big switch! All incoming messages at the server
#       will be fed into this procedure.  Recognised messages are
#       forwarded to separate procedures, where errors are sent back
#       at once, if relevant. Responses to messages are sent from this
#       procedure to highlight the internal mechanisms of the
#       protocol.
#
# Arguments:
#	c	Identifier of client, as created in Accept
#	msg	Message to treat
#
# Results:
#       None.
#
# Side Effects:
#       Will handle connection, disconnections and subscriptions!!
proc ::stomp::server::Dispatch { c msg } {
    variable STOMP
    variable version

    if { ![IsA $c client] } {
	return -code error "$c is not a known STOMP client connection"
    }
    upvar \#0 $c CLT
    upvar \#0 $CLT(server) SRV

    # Fetch data from incoming message and perform debug dump if
    # appropriate.
    set cmd [[namespace parent]::message::getCommand $msg]
    array set HDR [[namespace parent]::message::getHeaders $msg]
    Debug 4 "Receive $cmd"
    foreach k [array names HDR] {
	Debug 5 "\t${k}:$HDR($k)"
    }

    # Check if connection has been properly established (we've
    # properly negotiated a version number then) and scream otherwise.
    if { $cmd ne "STOMP" && $cmd ne "CONNECT" && $CLT(version) < 0 } {
	Debug 2 "Refused message in non-connected state, security breach\
                 attempt?"
	SendError $c "Connection not fully established yet!"
	return ""
    }

    switch $cmd {
	"STOMP" -
	"CONNECT" {
	    set protoVersion [HandleConnect $c $msg]
	    if { $protoVersion >= 0 } {
		# Send Receipt if requested
		SendReceipt $c $msg

		# Generate CONNECTED information and send it back
		set sessionId [string map [list _ -] [namespace tail $c]]
		set response [[namespace parent]::message::new \
				  CONNECTED $CLT(factory)]
		[namespace parent]::message::setHeader $response \
		    heart-beat [join [list $SRV(-heartbeat-out) \
					  $SRV(-heartbeat-in)] ","] \
		    version $protoVersion \
		    session $sessionId \
		    server [string map [list "%version%" $version] \
				$STOMP(serverName)]
		Send $c $response
	    }
	}
	"SUBSCRIBE" {
	    set s [SubscriptionAdd $c $msg]
	    if { $s ne "" } {
		# Send Receipt if requested
		SendReceipt $c $msg
	    }
	}
	"UNSUBSCRIBE" {
	    SubscriptionRemove $c $msg
	    # Send Receipt if requested
	    SendReceipt $c $msg
	}
	"SEND" {
	    # Find destination of message
	    set destination \
		[[namespace parent]::message::getHeader $msg destination]
	    set d [Destination $CLT(vhost) $destination]

	    # Fan-out to all subscribers of that destination
	    if { $d ne "" } {
		DeliverMessage $d $msg
	    }

	    # Send Receipt if requested
	    SendReceipt $c $msg
	}
	"DISCONNECT" {
	    # Send Receipt at once
	    SendReceipt $c $msg
	    
	    Disconnect $c
	}
    }

    # Delete incoming message, we are done.
    catch {::stomp::message::delete $msg}
}


# ::stomp::server::Allowed -- Check if IP allowed to connect
#
#       Check if IP address of incoming client is allowed to connect
#       to the server according to the filters in the -allow and -deny
#       options.
#
# Arguments:
#	s	Identifier of server as returned by new
#	addr	Address of incoming client.
#
# Results:
#       1 if client allowed to connect further, 0 otherwise.
#
# Side Effects:
#       None.
proc ::stomp::server::Allowed { s addr } {
    variable STOMP

    if { ![IsA $s server] } {
	return -code error "$s is not a known STOMP server context"
    }
    upvar \#0 $s SRV

    if { [llength $SRV(-allow)] > 0 || [llength $SRV(-deny)] > 0 } {
	set allowed 0
	foreach ptn $SRV(-allow) {
	    if { [string match -nocase $ptn $addr] } {
		set allowed 1
	    }
	}
	if { $allowed } {
	    foreach ptn $SRV(-deny) {
		if { [string match -nocase $ptn $addr] } {
		    return 0
		}
	    }
	    return 1
	}
	return 0
    }
    return 1
}


# ::stomp::server::Accept -- Accept client connections
#
#       Accept (or reject) incoming client connections. As long as the
#       client's IP address is allowed, a client context is created
#       and initiated, letting the client pursue with CONNECT
#       information if necessary.
#
# Arguments:
#	s	Identifier of the server, as created by new
#	sock	Socket connection to (new) client
#	addr	IP address of remote client.
#	port	Port number to client
#
# Results:
#       None.
#
# Side Effects:
#       Will accept or reject connection according to incoming IP
#       address and settings at server.  Accepted connections will
#       wait for CONNECT message for further initialisation to be
#       performed.
proc ::stomp::server::Accept { s sock addr port } {
    variable STOMP

    if { ![IsA $s server] } {
	return -code error "$s is not a known STOMP server context"
    }
    upvar \#0 $s SRV

    # Check incoming IP address of client against list of allowed
    # addresses and close connection at once if rejected.
    if { ! [Allowed $s $addr] } {
	catch {close $sock}
	Debug 2 "Connection from ${addr}:${port} refused"
    }

    # Otherwise create a client context, setting up a socket factory
    # for the sending of messages, but also arranging for Dispatch to
    # be called whenever a new STOMP message has been received.
    set id [Identifier];  # Generate identifier for client
    set c [namespace current]::client_$SRV(id)_$id
    upvar \#0 $c CLT

    set CLT(-addr) $addr;
    set CLT(-port) $port;

    fconfigure $sock \
	-blocking 0 \
	-translation {auto lf} \
	-encoding utf-8

    set CLT(id) $id
    set CLT(server) $s
    set CLT(vhost) ""
    set CLT(sock) $sock
    set CLT(hb-in) 0
    set CLT(hb-out) 0
    set CLT(version) -1
    set CLT(factory) [[namespace parent]::message::factory $sock \
			  -type server]
    set CLT(reader) [[namespace parent]::message::reader $CLT(factory) \
			 [list [namespace current]::Dispatch $c] \
			 [list [namespace current]::ReaderError $c]]

    Debug 3 "Accepted new incoming connection from ${addr}:${port}"
}


# ::stomp::server::new -- Create new STOMP server
#
#       Create a new STOMP server that will listen for incoming
#       connections and dispatch messages according to client
#       subscriptions.  The procedure takes a number of dash-led
#       options which are as follows:
#       -port          Port number on which to listen to
#       -vhost         List of virtual host that we implement, empty for
#                      one default, called localhost.
#       -allow         List of IP address filters allowed to connect
#       -deny          List of IP address filters to deny connection from
#       -heartbeat-in  Heartbeat that we wish to have with clients.
#       -heartbeat-out Max heartbeat that we can handle.
#       -users         List of colon separated username and passwords
#                      authorised to connect to server.
#       -forgiveness   Forgiveness when disconnecting clients because of
#                      missed heartbeat (will factor up handshaked
#                      heartbeat).
#       -socketCmd     Command to use to establish socket listening connections
#
# Arguments:
#	args	List of dash-led options and arguments, see above.
#
# Results:
#       Return an identifier for the stomp server connection.  Empty
#       string on errors.
#
# Side Effects:
#       Will create a basic STOMP server
proc ::stomp::server::new { { args {}} } {
    variable STOMP

    set id [Identifier]
    set s [namespace current]::server_$id
    upvar \#0 $s SRV

    # Get options from arguments
    foreach opt [array names STOMP -*] {
	GetOpt args $opt SRV($opt) $STOMP($opt)
    }

    # Decide upon socket command to use, we understand the empty
    # string as the plain and regular socket!
    if { $SRV(-socketCmd) eq "" } {
	set sockCmd [list socket]
    } else {
	set sockCmd $SRV(-socketCmd)
    }

    # Start listening to incoming socket connection. Take into account
    # externally provided socket commands, so we can listen using TLS
    # for example.
    set SRV(id) $id
    if { [catch {eval [linsert $sockCmd end -server \
			   [list [namespace current]::Accept $s] \
			   $SRV(-port)]} sock] } {
	Debug 1 "Could not listen for incoming connections on port\
                 $SRV(-port): $sock"
	unset SRV
	return ""
    }
    set SRV(sock) $sock

    # Create virtual hosts, make sure we have a default one, i.e. one
    # listening on default field, usually localhost.
    if { [llength $SRV(-vhosts)] == 0 } {
	set SRV(-vhosts) [list $STOMP(default)]
    }
    foreach vhost $SRV(-vhosts) {
	set id [Identifier]
	set vh [namespace current]::vhost_$SRV(id)_$id
	upvar \#0 $vh VHOST
	
	set VHOST(id) $id
	set VHOST(server) $s
	set VHOST(-name) $vhost
    }

    Debug 4 "Listening for incoming STOMP connection on port $SRV(-port)"

    return $s
}
	
package provide stomp::server $::stomp::server::version
