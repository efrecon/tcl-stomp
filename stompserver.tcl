#! /usr/bin/env tclsh

set prg_args {
    -h        ""          "Print this help and exit"
    -v        0           "Verbosity level \[0-5\]"
    -port     61613       "Port to listen on"
    -users    {}          "List of authorised user and passwords, colon separated"
    -vhosts   {}          "List of virtual hosts"
    -tls      false       "Encrypt traffic using TLS? (boolean or strict)"
    -cafile   ""          "Path to CA file, if relevant"
    -certfile ""          "Path to cert file, if relevant"
    -keyfile  ""          "Path to key file, if relevant"
}



set dirname [file dirname [file normalize [info script]]]
set appname [file rootname [file tail [info script]]]
lappend auto_path [file join $dirname lib stomp]

package require stomp::server

# Dump help based on the command-line option specification and exit.
proc ::help:dump { { hdr "" } } {
    global appname

    if { $hdr ne "" } {
	puts $hdr
	puts ""
    }
    puts "NAME:"
    puts "\t$appname - A simple no-fuss STOMP server"
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

array set SRV {}
foreach {arg val dsc} $prg_args {
    set SRV($arg) $val
}

if { [::getopt argv -h] } {
    ::help:dump
}
for {set eaten ""} { $eaten ne $argv } {} {
    set eaten $argv
    foreach opt [array names SRV -*] {
	::getopt argv $opt SRV($opt) $SRV($opt)
    }
}

# Arguments remaining?? dump help and exit
if { [llength $argv] > 0 } {
    ::help:dump "$argv are unknown arguments!"
}

::stomp::verbosity $SRV(-v)
if { [string equal -nocase $SRV(-tls) strict] || [string is true $SRV(-tls)] } {
    package require tls
    set req [string equal -nocase $SRV(-tls) strict]
    set SRV(server) [::stomp::server::new \
			 -port $SRV(-port) \
			 -users $SRV(-users) \
			 -vhosts $SRV(-vhosts) \
			 -socketCmd [list ::tls::socket \
					 -require $req \
					 -tls1 1 \
					 -cafile $SRV(-cafile) \
					 -certfile $SRV(-certfile) \
					 -keyfile $SRV(-keyfile) \
					 -command ::tls::callback]]
} else {
    set SRV(server) [::stomp::server::new \
			 -port $SRV(-port) \
			 -users $SRV(-users) \
			 -vhosts $SRV(-vhosts)]
}


vwait forever
