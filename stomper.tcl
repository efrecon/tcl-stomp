#! /usr/bin/env tclsh

set prg_args {
    -?        ""          "Print this help and exit"
    -verbose  0           "Verbosity level \[0-5\]"
    -port     61613       "Port to send to"
    -host     localhost   "Hostname of remote server"
    -user     ""          "Username to authenticate with"
    -password ""          "Password to authenticate with"
    -topic    ""          "Topic to send messages to"
    -tls      false       "Encrypt traffic using TLS?"
    -cafile   ""          "Path to CA file, if relevant"
    -certfile ""          "Path to cert file, if relevant"
    -keyfile  ""          "Path to key file, if relevant"
}



set dirname [file dirname [file normalize [info script]]]
set appname [file rootname [file tail [info script]]]
lappend auto_path [file join $dirname lib stomp]

package require stomp::client

# Dump help based on the command-line option specification and exit.
proc ::help:dump { { hdr "" } } {
    global appname

    if { $hdr ne "" } {
	puts $hdr
	puts ""
    }
    puts "NAME:"
    puts "\t$appname - A STOMP forwarder, line@stdin --> STOMP topic"
    puts ""
    puts "USAGE"
    puts "\t${appname}.tcl \[options\]"
    puts ""
    puts "OPTIONS:"
    foreach { arg val dsc } $::prg_args {
	puts "\t${arg}\t$dsc (default: ${val})"
    }
    exit
}

proc ::getopt {_argv name {_var ""} {default ""}} {
    upvar $_argv argv $_var var
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

array set FWD {}
foreach {arg val dsc} $prg_args {
    set FWD($arg) $val
}

if { [::getopt argv "-\\?"] } {
    ::help:dump
}

for {set eaten ""} {$eaten ne $argv} {} {
    set eaten $argv
    foreach opt [array names FWD -*] {
	::getopt argv $opt FWD($opt) $FWD($opt)
    }
}

# Arguments remaining?? dump help and exit
if { [llength $argv] > 0 } {
    ::help:dump "$argv are unknown arguments!"
}

proc ::forward {} {
    global FWD

    set line [gets stdin]
    if { $line ne "" } {
	::stomp::client::send $FWD(client) $FWD(-topic) \
	    -body $line \
	    -type text/plain
    }
    
    if { [eof stdin] } {
	::stomp::client::disconnect $FWD(client)
	exit
    }
}


proc ::init { msg } {
    fconfigure stdin -buffering line
    fileevent stdin readable ::forward
}

proc ::tlssocket { args } {
    global FWD

    if { [catch {eval [linsert $args 0 ::tls::socket \
			   -tls1 1 \
			   -cafile $FWD(-cafile) \
			   -certfile $FWD(-certfile) \
			   -keyfile $FWD(-keyfile)]} sock] == 0 } {
	fconfigure $sock -blocking 1 -encoding binary
	::tls::handshake $sock
	return $sock
    }
    return -code error $sock
}

::stomp::verbosity $FWD(-v)
if { [string is true $FWD(-tls)] } {
    package require tls
    set FWD(client) [::stomp::client::connect \
			 -host $FWD(-host) \
			 -port $FWD(-port) \
			 -user $FWD(-user) \
			 -password $FWD(-password) \
			 -socketCmd ::tlssocket]
} else {
    set FWD(client) [::stomp::client::connect \
			 -host $FWD(-host) \
			 -port $FWD(-port) \
			 -user $FWD(-user) \
			 -password $FWD(-password)]
}
::stomp::client::handler $FWD(client) ::init CONNECTED

vwait forever
