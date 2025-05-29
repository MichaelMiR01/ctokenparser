#!/usr/bin/tclsh

package provide ctokenparser 1.0

# code used from https://github.com/mirohs/headify
# partially convert by AI from https://www.codeconvert.ai/c-to-tcl-converter
# the rest is adapted manually

# param parser for c functions and structs
# spliting the paramlist by , or ; must be already done beforehand
# schema
# fun_dec:
# int myfunc (int a, const char* b, char *c);
# par: 
# (int a, const char* b, char *c)
# removing () and splitting by ,
# list {int a} {const char* b} {char *c}
# this is our raw material
# assertion: each param has the following form
# last part: variable name, maybe has a * as first char, then mark as pointer
# last-1 part: variable type, maybe with a * as last char --> pointer
# if last-1 part is * then mark as pointer, take last-2 part as variable type
# everything before this must be modifiers
# beware of using makros will fail, since we don't do makros


# todo
# parse modifiers like unsigned long etc. correctly (into CInv Types and into tcc4tcl types)
# parse stdint and often used types into cinv and tcc4tcl correctly or make a typedef at least
# Tcl_Obj *CONST objv[] gets not parsed correctly, since const is second, not first modifier
# 

# include ctokenizer.tcl
source [file join [file dirname [info script]]  "ctokenizer.tcl"]

namespace eval ctokenparser {
    
    variable DEBUG 0
    
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable basename "none"
    
    variable cinv_types
    variable cinv_callbacks
    
    variable writemode "puts" ;# puts, putfile, stringify
    variable writestring ""
    variable writefp ""
    variable basename

proc report_error {text} {
    variable actphrase
    variable acterror
    write_string "# ERROR: $text"
    write_string "# [regsub -all -- {[\r\n]} $actphrase {}]"
    set acterror 1
}
proc debug {str} {
    # redirected puts
    
    variable DEBUG
    
    if {$DEBUG>0} {
        puts "$str"
    }
}
proc write_string {str} {
    # redirected puts to get output to file or string
    variable writemode 
    variable writestring 
    variable writefp 
    switch -- $writemode {
        "puts" {
            puts $str
        }
        "putfile" {
            if {$writefp!=""} {
                puts $writefp $str
            }
        }
        "stringify" {
            append writestring "$str\n"
        }
        default {
            puts $str
        }
    }
}

proc set_writemode {mode {fname ""}} {
    # set writemode and evtl open file
    # controls write_string
    variable writemode 
    variable writestring 
    variable writefp 
    set writemode $mode
    if {$writefp!=""} {
        close $writefp
    }
    set writefile ""
    switch -- $writemode {
        "puts" {
            #
        }
        "putfile" {
            if {$fname==""} {
                #error
                set writemode "puts"
            } else {
                set writefile $fname
                set writefp [open $fname w]
            }
        }
        "stringify" {
            #
        }
    }
}

proc lookahead {tokenstream tokcount} {
    # look ahead until the next semicolon (tok sem:)
    set tokens {}
    set token ""
    while {1} {
        # phr: actually can act as a specifier for the next element
        set nextpair [lindex $tokenstream $tokcount]
        incr tokcount
        lassign $nextpair token line
        if {$token=="phr:"} {
            break
        }
        if {$token != "whi:"} {
            lappend tokens [list $token $line]
        }
    }
    return $tokens
}

proc init {} {
    # init ctokenparser
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable cinv_types
    variable cinv_callbacks
    variable writemode 
    variable writestring 
    variable writefp 

    set writemode "puts" ;# puts, putfile, stringify
    set writestring ""
    set writefp ""

    set outstructs {}
    set outenums {}
    set outfuncs {}
    set outtypes {}
    set outtypedefs {}
    
    set cinv_types {}
    set cinv_callbacks {}
    
    # variable var to localize error messages
    set actphrase "-"
    set acterror 0
    
    set allParams {}
    set allStructs {}
    set allEnums {}
    set allTypes {}
    
}

proc readdefs {{fname headify.def}} {
    # read back intermediate format coming from tokenstream
    # unused
    set rawtokenstream ""
    set infile [open $fname r]
    set rawtokenstream [read $infile]
    close $infile
    return $rawtokenstream
}

proc parse_defs {rawstream} {
    # parse a raw tokenstream from tokenizer
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    set src     ""
    set par     ""
    set cur     ""
    set structname ""
    set lasttok ""
    set token ""
    set acttype ""
    set isstruct  0
    set iscur   0
    
    # variable var to localize error messages
    set actphrase "-"
    set acterror 0
    
    set allParams {}
    set allStructs {}
    set allEnums {}
    set allTypes {}
    
    set tokenstream {}
    
    set token ""
    set tokval ""
    
    set lines [split $rawstream \n]
    set endofstring [llength $lines]
    set actline 0
    
    while {$actline<$endofstring } {
       set line [lindex $lines $actline]
       set istoken [string index $line 3]
       if {$istoken==":"} {
           if {$token!=""} {
               #debug "add [list $token $tokval]"
               set tokval [string trim $tokval]
               lappend tokenstream [list $token $tokval]
               set token ""
               set tokval ""
           }
           set token [string range $line 0 3]
           append tokval "[string trim [string range $line 4 end]]\n"
       } else {
           append tokval "[string trim $line]\n"
       }
       incr actline
    }
    if {$token!=""} {
       #debug "add [list $token $tokval]"
       set tokval [string trim $tokval]
       lappend tokenstream [list $token $tokval]
       set token ""
       set tokval ""
    }
    return $tokenstream
}

proc split_params {paramstring} {
    # split params from tokenizer (tok:par)
    variable actphrase
    variable acterror
    variable allTypes
    variable allParams
    variable outtypes
    # first remove the surrounding trash
    set paramstring [string trim $paramstring "\n ;"]
    set paramstring [string range $paramstring [string first ( $paramstring]+1 [string last ) $paramstring]-1]
    set openbracket [string first "(" $paramstring]
    set closebracket [string first ")" $paramstring $openbracket]
    if {$openbracket>-1} {
        # handle funcdefs
        set paramstring2 [string map {( " " ) " "} [string range $paramstring 0 $closebracket-1]]
        append paramstring2 
        set openbracket [string first "(" $paramstring $closebracket]
        set closebracket [string first ")" $paramstring $openbracket]
        
        set dropped [string range $paramstring $openbracket $closebracket]
        write_string "# WARNING: Couldn't parse parameterstring properly from [join $actphrase { }]"
        write_string "# WARNING: nested typedef? Dropping [join $dropped { }] in parameterstring  [join $paramstring { }]";
        # this could mean we found a implizit typedef of a callback function
        # first stem functionname
        #debug "Parsing $paramstring2 |dropped $dropped| orig:$paramstring"
        set func_def [split_params "($paramstring2)"]
        set func [lindex $func_def end]
        if {[llength $func]>1} {
            set func_name [lindex $func end-2]
            set func_type [lindex $func end-1]
        }
        set func_params [split_params $dropped]
        write_string "# typedef $func_type $func_name ($func_params)"
        lappend allTypes $func_name [list $func_type $func_name $func_params]
        #dict set allParams $func_name [list $func_type $func_params]
        # add, so it appears in outtypes
        lappend outtypes $func_name
        
        set paramstringrest [string map {( " " ) " "} [string range $paramstring $closebracket+1 end]]
        append paramstring2 $paramstringrest
        #puts "paramstring2: $paramstring2"
        set paramstring $paramstring2
    }
    set params [split $paramstring ,]
    set plist ""
    foreach param $params {
        set param [string trim $param " \n;"]
        if {$param !=""} {
            lappend plist $param
        }
    }
    set preturn {}
    foreach elem $plist {
        lappend preturn [parse_param $elem]
    } 
    return $preturn
}

proc split_struct {structstring} {
    # simple case, no substructs
    # first split for lines and remove trailing comments
    set structlist_0 [string trim $structstring "{}\n ;"]
    if {[string first "\{" $structlist_0]>-1} {;# \}
        report_error "Nested structs not supported"
        return {}
    }
    set structlist_0 [split $structlist_0 "\n;"]
    set newstruct ""
    foreach structelem $structlist_0 {
        #debug "parsing $structelem"
        set structelements [split $structelem ";"]
        foreach subelem $structelements {
            set subelem [string trim $subelem]
            if {[string index $subelem 0]=="/"} {
                # comment, drop this
                set subelem ""
            } 
            if {$subelem!=""} {
                lappend newstruct $subelem
            }
        }
    }
    #debug "transform $structlist_0 --> $newstruct"
    set preturn {}
    foreach elem $newstruct {
        lappend preturn [parse_param $elem]
    } 
    return $preturn
}

proc split_enum {structstring} {
    # simnple case, no substructs
    # first split for lines and remove trailing comments
    set structlist_0 [string trim $structstring "{}\n ;"]
    if {[string first "\{" $structlist_0]>-1} {;# \}
        report_error "Nested structs not supported"
        return {}
    }
    set structlist_0 [split $structlist_0 "\n;"]
    set newstruct ""
    foreach structelem $structlist_0 {
        #debug "parsing $structelem"
        set structelements [split $structelem ";"]
        foreach subelem $structelements {
            set subelem [string trim $subelem]
            if {[string index $subelem 0]=="/"} {
                # comment, drop this
                set subelem ""
            } 
            if {$subelem!=""} {
                lappend newstruct $subelem
            }
        }
    }
    #debug "transform $structlist_0 --> $newstruct"
    set preturn {}
    foreach elem $newstruct {
        lappend preturn $elem
    } 
    return $preturn
}

proc parse_param {paramin} {
    # paramin is usually separated by spaces, so this is already a valid list implementation in TCL
    # return [list $varname $vartype $paramlist] 
    #debug "parsing $paramin"
    set l [llength $paramin]
    if {$l<2} {
        if {$paramin=="..."} {
            report_error "Variadic funcs not supported"
            return {}
        }
        if {$paramin!="void"} {
            set paramname [regsub -all -- {[^0-9a-zA-Z.-]} $paramin "_"] 
            write_string "# WARNING: $paramin has no vartype/varname, replacing with $paramin $paramname"
            lappend paramin $paramname
        } else {
            return [list void "" ""]
        }
    }
    # reverse list, we work from back to forward
    set paramlist [lreverse $paramin]
    # init
    set varname ""
    set varptr 0
    set vararray 0
    set vartype ""
    set varmodifiers ""
    set c 0
    # pop varname from list
    set varname [lindex $paramlist 0]
    set paramlist [lreplace $paramlist 0 0]
    # check for err
    if {$varname==""} {
        report_error "varname is empty ($paramin)"
    }
    # check for ptr
    if {[string index $varname 0]=="*"} {
        set varptr [regexp -all  {[*]} $varname] 
        set varname [string trim $varname *]
    }
    # TODO: parse for [] array modifiers
    if {[string index $varname end]=="\]"} {
        #debug "Got Array $varname"
    }
    
    #check for modifiers
    set cvmodifiers {const volatile cdecl __cdecl} 
    set intmodifiers {long short signed unsigned}
    set mods {}
    #debug "varname:  $varname"
    # get vartype
    # pop vartype from stack
    # check for single * also 
    while {$vartype==""} {
        if {[llength $paramlist]==0} {
            write_string "#WARNING: no vartype found for $paramin"
            break;
        }
        
        set vartype [lindex $paramlist 0]
        set paramlist [lreplace $paramlist 0 0]
            
        if {$vartype=="*"} {
            set varptr 1
            set vartype ""
        }
        if {[string first * $vartype]>-1} {
            set varptr [regexp -all  {[*]} $vartype] 
            set vartype [string trim $vartype *]
        }
        if {[lsearch -exact $cvmodifiers [string tolower $vartype]]>-1} {
            # got a modifier
            #debug "got modifiers $vartype"
            lappend mods $vartype
            set vartype ""
        }
        if {[lsearch -exact $intmodifiers [string tolower $vartype]]>-1} {
            # got a modifier
            #debug "got modifiers $vartype"
            lappend mods $vartype
            set vartype "int"
        }
    }
    if {[string index $vartype end]=="*"} {
        set varptr [regexp -all  {[*]} $vartype] 
        set vartype [string trim $vartype *]
    }
    #debug "vartype:  $vartype"
    #debug "varptr:  $varptr"
    #debug "REST: $paramlist"
    if {$varptr>0} {
        for {set i 0} {$i<$varptr} {incr i} {
            set vartype "$vartype*"
        }
    }
    foreach mod $mods {
        lappend paramlist $mod
    }
    #debug "result: [list $varname $vartype $paramlist] "
    return [list $varname $vartype $paramlist] 
}

proc pointerizeCType {_vartype {varname ""}} {
    # used for cinvoke, will turn simply type* into ptr.type
    # and convert some common cinvoke types like string
    variable cinv_types
    variable cinv_callbacks
    variable allTypes
    set knownTypes {
        char* char*
        void* ptr
        char** ptr
        Tcl_Interp* Tcl_Interp*
        Jim_Interp* Jim_Interp*
        Tcl_Obj* Tcl_Obj*
        Jim_Obj* Jim_Obj*
    }
    
    if {[lsearch -exact $knownTypes $_vartype]>-1} {
        set vartype [string map $knownTypes $_vartype]
        return $vartype
    } 
    set vartype $_vartype
    if {[lsearch -exact $cinv_types [string trim $vartype *]]>-1} {
        debug "# typedef $vartype to [string trim $vartype *]"
        set vartype [string trim $vartype "* "]
        return  "ptr.$vartype"
    }
    if {[lsearch -exact $cinv_callbacks [string trim $_vartype *]]>-1} {
        debug "# cinv_callbacks $_vartype to tclproc"
        set vartype [string trim $vartype "* "]
        return  "ptr.$vartype"
    }
    if {[lsearch -exact $cinv_callbacks [string trim $varname *]]>-1} {
        # check if vartype is identical
        set functypedef [dict get $allTypes $varname]
        set functype [lindex $functypedef 0]
        
        debug "# cinv_callbacks $_vartype == $functype $varname to tclproc"
        set functype [string trim $varname "* "]
        return  "ptr.$functype"
    }
    
    if {[string first * $vartype]>-1} {
        set vartype [string trim $vartype "* "]
        set vartype "ptr.$vartype"
    }
    return $vartype
}    

proc pointerizeVartype {_vartype {varname ""}} {
    # used for cinvoke, will turn simply type* into ptr.type
    # and convert some common cinvoke types like string
    variable cinv_types
    variable cinv_callbacks
    variable allTypes
    set knownTypes {
        char* string
        void* ptr
        char** ptr
        void {}
    }
    set vartype [string map $knownTypes $_vartype]
    
    if {[lsearch -exact $cinv_types [string trim $vartype *]]>-1} {
        debug "# typedef $vartype to [string trim $vartype *]"
        return  [string trim $vartype *]
    }
    if {[lsearch -exact $cinv_callbacks [string trim $_vartype *]]>-1} {
        debug "# cinv_callbacks $_vartype to tclproc"
        return  "tclproc"
    }
    if {[lsearch -exact $cinv_callbacks [string trim $varname *]]>-1} {
        # check if vartype is identical
        set functypedef [dict get $allTypes $varname]
        set functype [lindex $functypedef 0]
        
        debug "# cinv_callbacks $_vartype == $functype $varname to tclproc"
        return  "tclproc"
    }
    
    if {[string first * $vartype]>-1} {
        set vartype [string trim $vartype "* "]
        set vartype "ptr.$vartype"
    }
    return $vartype
}    

proc convertTokenstream {tokenstream} {
    # converts a tokenstream from ctokenizer
    # and sets the variable arrays 
    # wich can be used to reassemble cinvoke and tcc4tcl code
    
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable basename
    
    set src     ""
    set par     ""
    set cur     ""
    set structname ""
    set lasttok ""
    set token ""
    set acttype ""
    set isstruct  0
    set iscur   0
    
    # variable var to localize error messages
    set actphrase "-"
    set acterror 0
    
    set allParams {}
    set allStructs {}
    set allEnums {}
    set allTypes {}    

    set tokcount 0
    set par ""
    
    if {$basename!=""} {
        write_string "# parsing $basename"
    }

    write_string "# Tokenstream length [llength $tokenstream]"
    
    #debug "\n$tokenstream\n"
    while {[llength $tokenstream]>$tokcount} {
        #debug "getting token $tokcount"
        if {$acterror>0} {
            #debug "Caught error"
            set acterror 0
        }
        set nextpair [lindex $tokenstream $tokcount]
        incr tokcount
        lassign $nextpair token line
       if {$token=="phr:"} {
           #debug "phrase: $line"
           set actphrase $line
           set acterror 0
       }
    
       if {$token=="fun:"} {
           set acttype "function"
       }
       if {$token=="par:"} {
           # get params
           append par $line\n
       } else {
           # end par
           if {$par != ""} {
               #debug "Got par $lasttok $par "
               set funcname [lindex $lasttok end]
               set retptr 0
               set funcret [lindex $lasttok end-1]
               if {$funcret=="*"} {
                   set retptr 1
                   set funcret "[lindex $lasttok end-2]*"
               }
               set lasttok ""
               if {$acttype == "function"} {
                    if {$acterror==0} {
                        dict set allParams $funcname [list $funcret $par]
                        set acttype ""
                    } else {
                        write_string "# dropping $funcname"
                    }
               }
           }
           set par ""
       }
       if {($token=="tok:")} {
           # get params
           #debug "Token: $line"
           lappend lasttok $line
           if {$line == "typedef"} {
               set isstruct 0
               set isenum 0
               # look forward
               set tkc $tokcount;# remember just in case we need to go back
               set deftoks [lookahead $tokenstream $tokcount]
               #debug "# typedef $deftoks"
               
               # parse tokens here
               set cur ""
               set structname ""
               set structdef ""
               set structpars ""
               set isfuncptr 0
               set c 0
               foreach tokpair $deftoks {
                   lassign $tokpair tok val
                   set val [string trim $val]
                   incr c
                   switch $tok {
                       "tok:" {
                           if {$val=="struct"} {
                               set isstruct 1
                           }
                           if {$val=="enum"} {
                               set isenum 1
                               # should drop through now to line==enum
                               break
                           }
                           lappend structdef $val
                           set structname $val                           
                       }
                       "cur:" {
                           set cur [string trim $val "\{\}"]
                           if {$isenum>0} {
                               set _cur $cur
                           } else {
                               set _cur [split_struct $cur]
                           }
                           lappend structdef $_cur
                       } 
                       "par:" {
                           set _cur [split_params $val]
                           lappend structdef $_cur
                           lappend structpars $val
                           if {[llength $structpars]>1} {
                               set isfuncptr 1
                           }
                       } 
                       "sem:" {}
                       default {
                           lappend structdef $val
                       }
                   }
               }
               # typename is last token
               set structname [string trim $structname " ;"]
               if {$isenum>0} {
                   #
               } else {
                   #
                   incr tokcount [llength $deftoks]
               }
               # handle function pointer typedefs like
               # typedef void (*SignalHandler)(int signum)
               # typedef void* (*SignalHandler)(int signum)
               # typedef *(*SignalHandler)(int signum)
               # typedef float(*pt2Func)(float, float);
               if {($isfuncptr>0)||($structname=="*")||($structname=="void")} {
                   write_string "# WARNING $structname is not a valid type, trying to replace"
                   if {[llength $structpars]>0} {
                       set guessname [lindex $structpars 0]
                       if {[llength $guessname]>0} {
                           set structname [string trim [lindex $guessname end] " ();*"]
                       }
                       write_string "# INFO new typename  [expr {$isfuncptr>0?{function}:{}}] $structname $structpars"
                   } else {
                       write_string "# WARNING invalid typedef $structname $structdef"
                   }
                   if {$isfuncptr>0} {
                       # drop 
                       set structdef [lrange $structdef 0 end-[llength $structpars]]
                       set newpars [lrange $structpars 1 end]
                       lappend structdef [string trim [lindex $structpars 0] "; "]
                       foreach par $newpars {
                           lappend structdef [split_params $par]
                       }
                       #lset structdef 0 $structname
                       write_string "# INFO typedef function $structname $structdef"
                       
                   }
                       
               }
               if {$structname!=""} {
                    if {$acterror==0} {
                        if {$isstruct==0} {
                            lappend allTypes $structname [string trim $structdef " ;"]
                        } else {
                            lappend allTypes $structname [list struct $structname]
                            if {(![dict exists $allStructs $structname])||([dict get $allStructs $structname]=="")} {
                                lappend allStructs $structname $cur
                            }
                        }
                    } else {
                        write_string "# dropping $structname"
                    }
               }
               
           }
           if {$line=="struct"} {
               set line ""
               set tkc $tokcount;# remember just in case we need to go back
               set deftoks [lookahead $tokenstream $tokcount]
               incr tokcount [llength $deftoks]
               # parse tokens here
               
               set cur ""
               set structname ""
               foreach tokpair $deftoks {
                   lassign $tokpair tok val
                   if {$tok=="cur:"} {
                       set cur [string trim $val "\{\}"]
                   } else {
                       append structname "$val "
                   }
               }
               set structname [string trim $structname " ;"]
               if {$structname!=""} {
                    if {$acterror==0} {
                        if {(![dict exists $allStructs $structname])||([dict get $allStructs $structname]=="")} {
                            write_string "# adding struct $structname"
                            lappend allStructs $structname $cur
                        }
                    } else {
                        write_string "# dropping $structname"
                    }
               }
           }
           if {$line=="enum"} {
               set line ""
               set tkc $tokcount;# remember just in case we need to go back
               set deftoks [lookahead $tokenstream $tokcount]
               incr tokcount [llength $deftoks]
               # parse tokens here
               
               set cur ""
               set structname ""
               foreach tokpair $deftoks {
                   lassign $tokpair tok val
                   if {$tok=="cur:"} {
                       set cur [string trim $val "\{\}"]
                   } else {
                       append structname "$val "
                   }
               }
               set structname [string trim $structname " ;"]
               if {$structname!=""} {
                    if {$acterror==0} {
                        lappend allEnums $structname $cur
                    } else {
                        write_string "# dropping $structname"
                    }
               }
           }
       } 
       incr lines
    }
    
    set outstructs {}
    set outenums {}
    set outfuncs {}
    set outtypes {}
    set outtypedefs {}
    
    debug "allParams $allParams"
    debug "allStructs $allStructs"
    debug "allEnums $allEnums"
    debug "allTypes $allTypes"
    
    foreach {name params} $allParams {
        set actphrase "$name ([join $params { }])"
        lassign $params funcret struct
        set paramlist [split_params $params]
        if {$acterror==0} {
            dict set outfuncs $name [list $funcret $paramlist]
        } else {
            write_string "# dropping $name"
            set acterror 0
        }
        foreach elem $paramlist {
            lassign $elem varname vartype params
            if {($vartype !="")&&([lsearch -exact $outtypes $vartype]<0)} {
                lappend outtypes $vartype
            }
        }
    }
    foreach {name struct} $allStructs {
        set actphrase "$name ([join $struct { }])"
        set _struct  [split_struct $struct]
        if {$acterror==0} {
            dict set outstructs $name $_struct
        } else {
            write_string "# dropping $name"
            set acterror 0
        }
    }
    foreach {name struct} $allEnums {
        set actphrase "$name ([join $struct { }])"
        set spl [split $struct ,]
        set enums {}
        foreach el $spl {
            set el [string trim $el]
            if {$el!=""} {
                lappend enums $el
            }
        }
        dict set outenums $name $enums 
    }
    foreach {name struct} $allTypes {
        set actphrase "$name ([join $struct { }])"
        dict set outtypedefs $name $struct
        
        #lappend outtypes $name
    }
    
}

proc print_phrase {phrase {nocur 0}} {
    # return phrase source code in c
    set phrase_text ""
    set phrase_elements {}
    if {[lindex $phrase 0] ne "extC"} {
        #append phrase_text "phr:\[[lindex $phrase 0]: "
        set e [lindex $phrase 2]
        set f [lindex $phrase 3]
        if {$e ne "" && $f ne ""} {
            set uid_first [dict get $e uid] 
            set uid_last [dict get $f uid]
            set n [dict get $e next]
            #puts "...from $uid_first to $uid_last"
            set phr_str ""
            while {($e!="")&&($n!="")} {
                set p [dict get $e begin]
                if {[dict get $e type]=="tok"} {
                    # keep this for later use as func name
                    set lasttok $p
                }
                if {($nocur)&&([lindex $phrase 0]=="fun_def")} {
                    if {[dict get $e type]=="cur"} {
                        set p ""
                    } else {
                        #add element
                    }
                }
                append phr_str "$p"
                if {$uid_first==$uid_last} {
                    # drop single elements, since they are comments or preprocs
                    # comment this out if you need these tokenized also
                    break
                }
                lappend phrase_elements $e

                if {$n==$uid_last} {
                    break
                }
                set n [dict get $e next]
                if {$uid_first == $uid_last} {
                    # only 1 element
                    break
                }
                set e [ctokenizer::get_element_by_uid $n]
                    
            }
            append phrase_text [string trim $phr_str]
            
        }
        #append phrase_text "\n"
        #foreach e $phrase_elements {
        #    set type [dict get $e type]
        #    set txt [dict get $e begin]
        #    append phrase_text "$txt\n"
        #}
    } else {
        #append phrase_text "phr:\[extern C suppressed\]\n"
    }
    if {[string trim $phrase_text]==""} {
        return ""
    }
    return $phrase_text
}

proc print_phrases {e} {
    # print a set of phrases from element
    # e is a linked list (e, e->next, ...) of elements
    while {$e != ""} {
        set e [ctokenizer::skip_whi_lbr_sem $e]
        if {$e == ""} {break}
        set phrase [ctokenizer::get_phrase $e]
        set next [dict get $e next]  
        set last [dict get [lindex $phrase 3] uid]
        set type ""
        set def {}
        set isptr 0
        set isarr 0
        set arrspec  ""
        while {$next<=$last} {
            set e [ctokenizer::skip_whi_lbr $e]
            if {$e == ""} break;
            lassign $e {} type {} begin {} end {} next  {} uid {}
            if {$type!="sem"} {
                switch -- $begin {
                    "*" {
                        set isptr 1
                    }
                    default {
                        if {$type=="bra"} {
                            set isarr 1
                            set arrspec [string trim $begin "\[\]"]
                        } else {
                            lappend def $type $begin
                            set varname $begin
                        }
                    }
                }
            }
            set e [ctokenizer::get_element_by_uid $next]
            if {$e == ""} break;
        }
        if {$type=="sem"} {
            set varspec [lrange $def 0 end-2]
            set vartype ""
            foreach {type text} $varspec {
                append vartype "[string trim $text {}] ";#\"
            }
            if {$isptr>0} {
                set vartype "$vartype*"
            }
            
            if {$isarr>0} {
                set arrspec "($arrspec)"
            }
            set vartype [pointerizeVartype $vartype $varname]
            write_string "    $vartype $varname$arrspec"
        }
        if {$e == ""} break;
        set e [ctokenizer::get_element_by_uid [dict get $e next]]
        
    }
}

proc headerize_phrase {phrase head} {
    # convert phrase back to c
    variable DEBUG
    lassign $phrase ptype ppub pfirst plast
    set phrasetext [print_phrase $phrase 1]
    set last [dict get $plast uid]
    
    set phrase [list type $ptype is_public $ppub first $pfirst last $plast]
    set last [dict get $plast uid]

    if {$DEBUG} {
        puts "phrase = [dict get $phrase type]"
        append head "phrase = "
        append head [dict get $phrase type]
        append head "\n"
    }
    if {[dict get $phrase type] eq "error"} {
        if {[dict exists $phrase last]} {
            set e [dict get $phrase last]
        }
        set line [count_line_breaks [dict get $list begin] [dict get $e end]] + 1
        puts "$basename:$line: Error"
        return
    }
    if {[string trim $phrasetext]!=""} {
        set first [string trim $phrasetext \n]
        if {$DEBUG} {
            append head [string range $first 0 end]
            append head "\n"
        }
        switch [dict get $phrase type] {
            "var_dec" {
                append head "extern "
                append head [string range $first 0 end]
                append head "\n"
            }
            "arr_dec" {
                append head "extern "
                append head [string range $first 0 end]
                append head "\n"
            }
            "fun_dec" {
                append head [string range $first 0 end]
                append head "\n"
            }
            "preproc" {
                append head [string range $first 0 end]
                append head "\n"
            }
            "fun_def" {
                append head [string range $first 0 end]
                append head ";\n"
            }
            "var_def" {
                append head "extern "
                append head [string range $first 0 end]
                append head ";\n"
            }
            "arr_def" {
                append head "extern "
                append head [string range $first 0 end]
                append head ";\n"
            }
            "struct_union_enum_def" {
                append head [string range $first 0 end]
                append head "\n"
            }
            "type_def" {
                append head [string range $first 0 end]
                append head "\n"
            }
            "line_comment" {
                append head [string range $first 0 end]
                append head "\n"
            }
            "block_comment" {
                append head [string range $first 0 end]
                append head "\n"
            }
            default {
                #append head "// phrase "
                #append head $first
                #append head " NOT HANDLED\n"
            }
        }
    }
    return $head
}

proc create_header {basename elist} {
    # helper to extract header from c source if necessary
    if {$elist eq ""} {
        # "list cannot be null"
        return ""
    }
    #puts "$elist"
    set head ""
    set protector [regsub -all -- {[^0-9a-zA-Z-]} $basename "_"]
    append head "#ifndef "
    append head $protector
    append head "_h_INCLUDED\n#define "
    append head $protector
    append head "_h_INCLUDED\n"
    # Phrase {type is_public first last}
    set e [dict get $elist [lindex $elist 0]]
    while {$e != ""} {
        set e [ctokenizer::skip_whi_lbr_sem $e]
        if {$e == ""} {break}
        set phrase [ctokenizer::get_phrase $e]
        append head [headerize_phrase $phrase ""]
        set e [lindex $phrase 3]
        if {$e == ""} break;
        set e [ctokenizer::get_element_by_uid [dict get $e next]]
    }
    append head "#endif\n"
    return $head
    
}

proc write_structlists {} {
    # write the parsed structs etc
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    
    write_string "\n# structs is list |name ||varname type modifiers| ... |"
    write_string "set [list structs $outstructs]\n"
    write_string "# outenums is list |name ||varname type modifiers| ... |"
    write_string "set [list enums $outenums]\n"
    
    write_string "# functions is list |name returntype ||varname type modifiers| ... |"
    write_string "set [list functions $outfuncs]\n"
    write_string "# typelist is list |name ... | of returntypes of functions"
    write_string "set [list typelist $outtypes]\n"
    write_string "# typedeflist is list |name |mixed definitions| ... |"
    write_string "set [list typedeflist $outtypedefs]\n"
}
proc write_tcc_funcs {} {
    # write function definitions for tcc4tcl
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    write_string "\n# implementation for tcc4tcl"
    write_string "if \{\[info exists handle\]\} \{\n"
    foreach {funcname funcspec} $outfuncs {
        lassign $funcspec funcret funcdef
        func2tcc4tcl $funcname $funcret $funcdef
    }
    write_string "\n\};# end tcc4tcl code\n"    
}

proc write_tcc_typedefs {} {
    # write typedefs for tcc4tcl
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable cinv_types
    variable cinv_callbacks

    write_string "\n# implementation for tcc4tcl"
    write_string "if \{\[info exists handle\]\} \{\n"; 
    
    set deftypes [dict keys $outtypedefs]
    write_string "# defined types are $deftypes"
    foreach typename $deftypes {
        set _typename [string trim $typename *]
        #debug "searching $_typename ($typename) in $outtypes"
        if {[lsearch -exact $outtypes $_typename]>-2} {
            set isptr [expr [string first * $typename]>-1]
            set typespec [dict get $outtypedefs $_typename]
            if {([llength $typespec]==2)&&([lindex $typespec 0]=="struct")} {
                if {[dict exists $allStructs $_typename]} {
                    set structdef [dict get $allStructs $_typename]
                    if {[string trim $structdef]!=""} {
                        write_string "\n#CStruct $_typename ";#\{
                        #ctokenizer::init
                        #set el [ctokenizer::get_elements "" $structdef]
                        #set e [lindex $el 0]
                        #print_phrases $e
                        #write_string "\}"
                    }
                }
                # handle it an opaque ptr to a struct
                #write_string "\ntypedef ptr $_typename"
                #write_string "typedef struct $_typename"
                #lappend cinv_types $_typename
            } elseif {([lsearch -exact $outtypes $_typename]==-1)&&([lsearch -exact $outtypes "$_typename*"]==-1)} {
                puts "# normal typedef $_typename $typespec"
            } else {
                # handle it as a function definiton
                write_string  "\n# typedef function $_typename as $typespec"
                #lappend cinv_callbacks $_typename
                set paramstart [lsearch -exact $typespec $_typename]
                set funcparams [lindex [lrange $typespec end end] 0]
                set funcret [lindex  $typespec 0]
                set funcret [pointerizeCType $funcret]    
                set paramstring ""
                set paramtypes ""
                set funcstring ""
                foreach param $funcparams {
                    lassign $param varname vartype varspecs
                    # we drop varspecs, since we can't handle this
                    append funcstring "$vartype $varname, "
                    set vartype [pointerizeCType $vartype]    
                    append paramstring "$varname "
                    append paramtypes "$vartype $varname "
                }      
                set paramstring [string trim $paramstring]
                set paramtypes [string trim $paramtypes]
                set funcstring [string trim $funcstring " ,"]

                write_string "\n\$handle ccode \{"
                write_string "      typedef [lindex  $typespec 0] $_typename ($funcstring);"
                write_string "\}"
                
                write_string "# overwrite proc cb_$_typename so it does usefull things"
                write_string "# use callback via \[tcc4tcl::lookup_Symbol c_cb_$_typename\] and converting it to a valid ptr"
                write_string "proc cb_$_typename \{$paramstring\} \{\};# prototype dummy"
                write_string "\$handle tclwrap cb_$_typename \"$paramtypes\" \"$funcret\" ";# prototype dummy"
                
                # the simpler definition would lead to artefacts 
                #write_string "\$handle proc cb_$_typename \{$paramtypes\} \"$funcret\" \{\};# prototype dummy"
                
                
            }
        }
    }
    write_string "\n\};# end tcc4tcl code\n"    
}

proc write_cinvoke_funcs {} {
    # write function definitions for CInvoke
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable cinv_types
    variable cinv_callbacks
    write_string "\n# implementation for cinvoke"
    write_string "if \{\[info command cinv_handle\]!=\"\"\} \{\n"
    foreach {funcname funcspec} $outfuncs {
        lassign $funcspec funcret funcdef
        func2cinvoke  $funcname $funcret $funcdef
    }
    write_string "\n\};# end CInvoke code\n"    
}

proc write_cinvoke_typedefs {} {
    # write type definitions for CInvoke
    variable allParams 
    variable allStructs 
    variable allEnums 
    variable allTypes
    variable actphrase
    variable acterror
    variable outstructs 
    variable outenums 
    variable outfuncs 
    variable outtypes 
    variable outtypedefs 
    variable cinv_types
    variable cinv_callbacks

    write_string "\n# typedefs for cinvoke"
    write_string "if \{\[info command cinv_handle\]!=\"\"\} \{\n"
    
    set deftypes [dict keys $outtypedefs]
    write_string "# defined types are $deftypes"
    foreach typename $deftypes {
        set _typename [string trim $typename *]
        #debug "searching $_typename ($typename) in $outtypes"
        if {[lsearch -exact $outtypes $_typename]>-2} {
            set isptr [expr [string first * $typename]>-1]
            set typespec [dict get $outtypedefs $_typename]
            if {([llength $typespec]==2)&&([lindex $typespec 0]=="struct")} {
                if {[dict exists $allStructs $_typename]} {
                    set structdef [dict get $allStructs $_typename]
                    if {[string trim $structdef]!=""} {
                        write_string "\nCStruct $_typename \{"
                        ctokenizer::init
                        set el [ctokenizer::get_elements "" $structdef]
                        set e [lindex $el 0]
                        print_phrases $e
                        write_string "\}"
                    }
                }
                # handle it an opaque ptr to a struct
                write_string "\ntypedef ptr $_typename"
                write_string "typedef struct $_typename"
                lappend cinv_types $_typename
            } elseif {([lsearch -exact $outtypes $_typename]==-1)&&([lsearch -exact $outtypes "$_typename*"]==-1)} {
                #puts "# normal typedef $_typename $typespec"
            } else {
                # handle it as a function definiton
                write_string  "\n# typedef function $_typename as $typespec"
                lappend cinv_callbacks $_typename
                set paramstart [lsearch -exact $typespec $_typename]
                set funcparams [lindex [lrange $typespec end end] 0]
                set funcret [lindex  $typespec 0]
                set funcret [pointerizeVartype $funcret]    
                set paramstring ""
                set paramtypes ""
                foreach param $funcparams {
                    lassign $param varname vartype varspecs
                    # we drop varspecs, since we can't handle this
                    set vartype [pointerizeVartype $vartype]    
                    append paramstring "$varname "
                    append paramtypes "$vartype "
                }      
                set paramstring [string trim $paramstring]
                set paramtypes [string trim $paramtypes]
                
                write_string "# overwrite proc cb_$_typename so it does usefull things"
                write_string "# use callback via \[$_typename getptr\]"
                write_string "proc cb_$_typename \{$paramstring\} \{\};# prototype dummy"
                write_string "CCallback $_typename cb_$_typename \"$funcret\" \"$paramtypes\"";# prototype dummy"
                
                
            }
        }
    }
    write_string "\n\};# end CInvoke code\n"    
}

# opaque handles 
#typedef ptr TCCState
#typedef ptr TCCSymbol

# typelist is list |name ... | of returntypes of functions
#set typelist {TCCReallocFunc* TCCState* char* void* TCCErrorFunc* int char** TCCBtFunc*}

# typedeflist is list |name |mixed definitions| ... |
#set typedeflist {tesint {int tesint} TCCReallocFunc {void * TCCReallocFunc {{ptr void* {}} {size int {unsigned long}}}} TCCState {struct TCCState} TCCErrorFunc {void TCCErrorFunc {{opaque void* {}} {msg char* const}}} TCCBtFunc {int TCCBtFunc {{udata void* {}} {pc void* {}} {file char* const} {line int {}} {func char* const} {msg char* const}}} symbol_cb {void* symbol_cb {{ctx void* {}} {name char* const} {val void* const}}}}

#typedef void TCCErrorFunc(void *opaque, const char *msg);
#CCallback TCC_ErrorFunc Tcc4tclErrorFunc "" {ptr char*}


proc func2tcc4tcl {funcname funcret funcparams} {
    # output functions for tcc4tcl
    # $handle cwrap tccimg_import {Tcl_Interp* interp char* im1 }  Tcl_Obj*  
    # rewrite funcparams (list of 3 elements: varname vartype varspecs)
    #debug "parsing $funcname $funcret $funcparams"
    set paramstring ""
    foreach param $funcparams {
        lassign $param varname vartype varspecs
        # we drop varspecs, since we can't handle this
        set vartype [pointerizeCType $vartype $varname]
        append paramstring "$vartype $varname "
    }      
    set funcret [pointerizeCType $funcret]
    set paramstring [string trim $paramstring ", "]
    set cproc "\$handle cwrap $funcname \{$paramstring\} $funcret 0"
    write_string "# $funcret $funcname ($funcparams)"
    write_string $cproc
    return $cproc
}

# rearrange into valid cinvoke notation
proc func2cinvoke {funcname funcret funcparams} {
    # output function for CInvoke
    #LIBTCCAPI void tcc_set_realloc(TCCReallocFunc *my_realloc);
    #libtcc function tcc_set_realloc tcc_set_realloc "" "tclproc"
    # rewrite funcparams (list of 3 elements: varname vartype varspecs)
    # todo: define ptr types
    set paramstring ""
    set funcparamstring ""
    foreach param $funcparams {
        lassign $param varname vartype varspecs
        # we drop varspecs, since we can't handle this
        set _vartype [pointerizeVartype $vartype $varname]    
        append paramstring "$_vartype "
        append funcparamstring "[join $varspecs { }] $vartype $varname, "  
    }      
    set paramstring [string trim $paramstring ", "]
    set funcparamstring [string trim $funcparamstring ", "]
    write_string "# $funcret $funcname ($funcparamstring)"
    set funcret [pointerizeVartype $funcret]    
    if {$funcret=="void"} {
        set funcret ""
    }
    set cproc "cinv_handle function $funcname $funcname \"$funcret\" \"$paramstring\" "
    write_string $cproc
    return $cproc
}

proc convert_c2h {fname {outfile ""}} {
    # parse a .c  file and extract a .h header
    # demonstrates usage of ctokenparser
    
    # first use tokenizer to tokenize the c-source
    ctokenizer::parse_file $fname "stringify"
    # init parser
    ctokenparser::init
    #if {$outfile!=""} {
    #    ctokenparser::set_writemode putfile $outfile
    #} else {
        ctokenparser::set_writemode stringify
    #}
    # parse back into tokenstream
    set ts [ctokenparser::parse_defs $::ctokenizer::writestring]
    # interpret as much as we can
    set ::ctokenparser::basename [file tail $fname]
    ctokenparser::convertTokenstream $ts
    
    # write header file if necessary
    # decide on file extension
    set basename ""
    if {$outfile!=""} {
        set basename [file rootname [file tail $outfile]]
    } else {
        if {[file extension [file tail $fname]]==".h"} {
            set basename ""
        } else {
            set basename [file rootname [file tail $fname]]
        }
    }
    
    set ::ctokenparser::basename $basename        
    set e $::ctokenizer::allElements
    set headerfile [::ctokenparser::create_header "$basename" $e]
    if {$basename !=""} {
        puts "# Writing header to ${basename}.h"
        set fp [open "${basename}.h" w]
        puts $fp $headerfile
        close $fp
    } else {
        return $headerfile
    }
    
    # done
}
proc convert_c2tcl {fname {outfile ""}} {
    # parse a .c or .h file and convert it into tcc4tcl and CInvoke notation
    # demonstrates usage of ctokenparser
    
    # first use tokenizer to tokenize the c-source
    ctokenizer::parse_file $fname "stringify"
    # init parser
    ctokenparser::init
    if {$outfile!=""} {
        ctokenparser::set_writemode putfile $outfile
    } else {
        ctokenparser::set_writemode puts
    }
    # parse back into tokenstream
    set ts [ctokenparser::parse_defs $::ctokenizer::writestring]
    # interpret as much as we can
    set ::ctokenparser::basename [file tail $fname]
    ctokenparser::convertTokenstream $ts
    
    #write out all found stuff
    ctokenparser::write_structlists
    
    ctokenparser::write_cinvoke_typedefs
    ctokenparser::write_cinvoke_funcs
    
    ctokenparser::write_tcc_typedefs
    ctokenparser::write_tcc_funcs
    
    # flush file output 
    ctokenparser::set_writemode puts
    # done
}

proc convert {fname {outfile ""}} {
    ctokenparser::convert_c2tcl $fname $outfile
    if {$outfile !=""} {
        puts [ctokenparser::convert_c2h $fname $outfile]
    }
}


}; #end namespace

# reactivate for commandline behaviour
if {0} {
    proc test_cconvert {fname {outfile ""}} {
        ctokenparser::convert_c2tcl $fname $outfile
        if {$outfile !=""} {
            puts [ctokenparser::convert_c2h $fname $outfile]
        }
    }
    
    proc rundir {dirname} {
        foreach fname [glob $dirname/*.h] {
            puts "working on $fname"
            update
            if {[catch {test_cconvert $fname "${fname}.tcl"} e]} {
                puts "Err in $fname: $e"
            }
        }
        foreach fname [glob $dirname/*.c] {
            puts "working on $fname"
            update
            if {[catch {test_cconvert $fname "${fname}.tcl"} e]} {
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
        test_cconvert {*}$argv
    }
}       
