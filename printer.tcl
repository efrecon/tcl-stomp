#! /usr/bin/env tclsh

set prg_args {
    -h        ""         "Print this help and exit"
    -v        0          "Verbosity level \[0-5\]"
    -port     61613      "Port to send to"
    -host     localhost  "Hostname of remote server"
    -user     ""         "Username to authenticate with"
    -password ""         "Password to authenticate with"
    -topic    ""         "Topic to send messages to"
    -type     ""         "Pattern of messages to accept and output on stdout, empty for all, or, e.g. text/*"
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
    puts "\t$appname - A STOMP printer, STOMP topic --> stdout"
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

array set PRT {}
foreach {arg val dsc} $prg_args {
    set PRT($arg) $val
}

if { [::getopt argv -h] } {
    ::help:dump
}
foreach opt [array names PRT -*] {
    ::getopt argv $opt PRT($opt) $PRT($opt)
}

# Arguments remaining?? dump help and exit
if { [llength $argv] > 0 } {
    ::help:dump "$argv are unknown arguments!"
}

proc ::incoming {msg} {
    global PRT

    set type [::stomp::message::getHeader $msg content-type]
    if { $PRT(-type) eq "" || [string match $PRT(-type) $type] } {
	puts stdout [::stomp::message::getBody $msg]
    }
}


proc ::init { msg } {
    global PRT

    ::stomp::client::subscribe $PRT(client) $PRT(-topic) -handler ::incoming
}

::stomp::verbosity $PRT(-v)
set PRT(client) [::stomp::client::connect \
		     -host $PRT(-host) \
		     -port $PRT(-port) \
		     -user $PRT(-user) \
		     -password $PRT(-password)]
::stomp::client::handler $PRT(client) ::init CONNECTED

vwait forever
