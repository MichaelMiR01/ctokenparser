#!/usr/bin/tclsh
catch {console show}
lappend auto_path ..
package req ctokenparser

# now we can source the automatically processed lib2include.tcl

ctokenparser::convert_c2tcl "lib2include.c" "lib2include.tcl"
ctokenparser::convert_c2h "lib2include.c" "lib2include.h"

