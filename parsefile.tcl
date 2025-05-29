#!/usr/bin/tclsh

# small usage example for ctokenparser.tcl 
lappend auto_path .
package require ctokenparser

proc cconvert {fname {outfile ""}} {
    ctokenparser::convert_c2tcl $fname $outfile
    if {$outfile !=""} {
        puts [ctokenparser::convert_c2h $fname $outfile]
    }
}

proc rundir {dirname} {
    foreach fname [glob $dirname/*.h] {
        puts "working on $fname"
        update
        if {[catch {cconvert $fname "${fname}.tcl"} e]} {
            puts "Err in $fname: $e"
        }
    }
    foreach fname [glob $dirname/*.c] {
        puts "working on $fname"
        update
        if {[catch {cconvert $fname "${fname}.tcl"} e]} {
            puts "Err in $fname: $e"
        }
    }
}

if {$argc==0} {
    return
}
if {[file isdir [lindex $argv 0]]} {
    rundir [lindex $argv 0]
} else {
    cconvert {*}$argv
}
