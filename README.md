ctokenparser is a simple c-header parser in pure tcl

parts of the code are taken from https://github.com/mirohs/headify
and translated to tcl

it trys to analyze functions, structs, typedefs and enums and convert them
to a tcl version for

- tcc4tcl
- TclCinvoke

It additionally produce a headerfile form a c source

sample usage: (see sample also)
```
#!/usr/bin/tclsh
#small usage example for ctokenparser.tcl 
lappend auto_path .
package require ctokenparser

proc cconvert {fname {outfile ""}} {
    ctokenparser::convert_c2tcl $fname $outfile
    if {$outfile !=""} {
        puts [ctokenparser::convert_c2h $fname $outfile]
    }
}


if {$argc==0} {
    return
}

cconvert {*}$argv

```


