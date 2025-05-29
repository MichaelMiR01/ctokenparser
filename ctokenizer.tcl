# code used from https://github.com/mirohs/headify
# partially convert by AI from https://www.codeconvert.ai/c-to-tcl-converter
# the rest is adapted manually

namespace eval ctokenizer {
    variable indent
    variable error_message
    variable error_pos
    variable element_uid
    variable allElements
    
    variable writemode "puts" ;# puts, putfile, stringify
    variable writestring ""
    variable writefp ""
    
# Structure to hold state information
# State {
#    input {}
#    phrase {}
#}

# ElementTypeName array
#set ElementTypeName {
#    "err" "whi" "tok" "pre" "lco" "bco" "sem" 
#    "lbr" "par" "bra" "cur" "clo" "asg" "pub" 
#    "eos" "ElementTypeCount" "exc"
#}
#set PhraseTypeNames {
#    "unknown" "error" "fun_dec" "fun_def" "var_dec" 
#    "var_def" "arr_dec" "arr_def" "struct_union_enum_def" 
#    "type_def" "preproc" "line_comment" "block_comment" "extC"
#}

proc init {} {
    # Init static variables
    variable indent
    variable error_message
    variable error_pos
    variable element_uid
    variable allElements
    variable writemode 
    variable writestring 
    variable writefp 

    set writemode "puts" ;# puts, putfile, stringify
    set writestring ""
    set writefp ""
    
    set indent true
    set error_message ""
    set error_pos ""
    
    set element_uid 0
    set allElements {}
}

proc write_string {str} {
    # redirected puts
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

proc parse_file {fname {mode puts} {outfile ""}} {
    variable indent
    variable error_message
    variable error_pos
    variable element_uid
    variable allElements
    variable writemode 
    variable writestring 
    variable writefp 
    #init
    init
    # read source
    set fp [open $fname r]
    set s [read $fp]
    close $fp
    
    # debug
    set el [get_elements "" $s]
    foreach e $el {
        set type [dict get $e type]
        set what [dict get $e begin]
        set uid [dict get $e uid]
        set next [dict get $e next]
        #debug "$type: $uid > $next   $what "
        #debug "next: $next [get_element_by_uid $next]"
    }
    #printout
    set_writemode $mode $outfile
    set e [dict get $allElements 0]
    print_phrases $e
}

# Function to make an Element
proc make_element {type begin end} {
    variable element_uid
    # hacked on this since the c code used char*, we have to subtract string length
    # and moreover, we have no pointers to elements for the next field
    # so we create a unique ID and store all elements in a global//namespace var
    
    set sl [expr [string length $begin]-[string length $end]]
    set str [string range $begin 0 $sl-1]
    set l [list "type" $type "begin" $str "end" $end "next" "" uid $element_uid]
    incr element_uid
    return $l
}

proc get_element_by_uid {uid} {
    variable allElements
    set e ""
    catch {
        set e [dict get $allElements $uid]
    }
    return $e
}

proc cstring_equal {s1 s2} {
    return [expr [string compare $s1 $s2]==0]
}
# Function to print an Element
proc print_element {e} {
    set type [lindex $e 1]
    set begin [lindex $e 3]
    set end [lindex $e 5]
    write_string "$type: [string range $begin 0 end]"
}

# Function to convert Element to string
proc string_element {e} {
    set type [lindex $e 1]
    set begin [lindex $e 3]
    set end [lindex $e 5]
    return "$type: [string range $begin 0 end]"
}

# Function to print Elements in a list
proc print_elements {list} {
    foreach e $list {
        print_element $e
    }
}

# Function to check if braces match
proc braces_match {bo bc} {
    return [expr {($bo == "(" && $bc == ")") || ($bo == "{" && $bc == "}") || ($bo == "[" && $bc == "]")}]
}

proc scan_next {s} {
    variable indent
    variable error_message
    variable error_pos
    if {$s==""} {
        return [make_element eos $s $s]
    }
    set c [string index $s 0]
    set d "\0"
    set t "\0"
    if {[string length $s]>0} {
        set t [string range $s 1 end]
        set d [string index $s 1]
    }
    set escape false
    #debug "scan switch s: $s t:$t c:$c d:$d"
    switch -- $c {
        "\0" {
            return [make_element eos $s $s]
        }
        "\n" {
            set indent true
            return [make_element lbr $s $t]
        }
        "*" {
            return [make_element [expr {$indent ? "pub" : "tok"}] $s $t]
        }
        ";" {
            set indent false
            return [make_element sem $s $t]
        }
        "=" {
            set indent false
            return [make_element asg $s $t]
        }
        "#" {
            if {!$indent} {
                return [make_element tok $s $t]
            }
            set indent false
            while {[string index $t 0] != "\0"} {
                if {[string index $t 0] == "\\" && [string index $t 1] == "\n"} {
                    set t [string range $t 2 end]
                    continue
                }
                if {[string index $t 0] == "\n"} {
                    break
                }
                set t [string range $t 1 end]
            }
            return [make_element pre $s $t]
        }
        " " {
            while {[string index $t 0] != "\0"} {
                if {[string index $t 0] == "\\" && [string index $t 1] == "\n"} {
                    set t [string range $t 2 end]
                    #set t [expr {$t + 2}]
                    continue
                }
                if {[string index $t 0] != " " && [string index $t 0] != "\t"} {
                    break
                }
                set t [string range $t 1 end]
            }
            return [make_element whi $s $t]
        }
        "\"" {
            set indent false
            set escape false
            while {[string index $t 0] != "\0"} {
                if {!$escape && [string index $t 0] == "\""} {
                    return [make_element tok $s [string range $t 1 end]]
                }
                if {[string index $t 0] == "\\"} {
                    set escape [expr {!$escape}]
                } else {
                    set escape false
                }
                set t [string range $t 1 end]
            }
            set error_message "unterminated string literal"
            set error_pos $s
            return [make_element err "$error_message $error_pos" ""]
        }
        "'" {
            set indent false
            set escape false
            while {[string index $t 0] != "\0"} {
                if {!$escape && [string index $t 0] == "'"} {
                    return [make_element tok $s [string range $t 1 end]]
                }
                if {[string index $t 0] == "\\"} {
                    set escape [expr {!$escape}]
                } else {
                    set escape false
                }
                set t [string range $t 1 end]

            }
            set error_message "unterminated character literal"
            set error_pos $s
            return [make_element err "$error_message $error_pos" ""]
        }
        "/" {
            if {[string index $t 0] == "/"} {
                set indent false
                set t [string range $t 1 end]

                while {[string index $t 0] != "\0"} {
                    if {[string index $t 0] == "\n"} {
                        break
                    }
                    set t [string range $t 1 end]

                }
                return [make_element lco $s $t]
            } elseif {[string index $t 0] == "*"} {
                set t [string range $t 1 end]

                while {[string index $t 0] != "\0"} {
                    if {[string index $t 0] == "*" && [string index $t 1] == "/"} {
                        return [make_element bco $s [string range $t 2 end]]
                    }
                    set t [string range $t 1 end]

                }
                set error_message "unterminated block comment"
                set error_pos $s
                return [make_element err "$error_message $error_pos" ""]
            } else {
                set indent false
                return [make_element tok $s $t]
            }
        }
        "(" {
            set indent false
            while {[string index $t 0] != "\0"} {
                set e [scan_next $t]
                set t [dict get $e end]
                if {[dict get $e type] == "eos"} {break}
                if {[dict get $e type] == "err"} {return $e}
                if {[dict get $e type] == "clo"} {
                    if {[braces_match $c [string index [dict get $e begin] 0]]} {
                        return [make_element par $s $t]
                    } else {
                        set error_message "braces do not match"
                        set error_pos "[dict get $e begin][dict get $e end]"
                        return [make_element err "$error_message $error_pos" ""]
                    }
                }
            }
            set error_message "unterminated braces \("
            set error_pos $s
            return [make_element err "$error_message $error_pos" ""]
        }
        "\{" {
            # \}
            set indent false
            while {[string index $t 0] != "\0"} {
                set e [scan_next $t]
                set t [dict get $e end]
                if {[dict get $e type] == "eos"} {break}
                if {[dict get $e type] == "err"} {return $e}
                if {[dict get $e type] == "clo"} {
                    return [make_element cur $s $t]
                }
            }
            set error_message "unterminated braces \{"
            set error_pos $s
            return [make_element err "$error_message $error_pos" ""]
        }
        "\[" {
            set indent false
            while {[string index $t 0] != "\0"} {
                set e [scan_next $t]
                set t [dict get $e end]
                if {[dict get $e type] == "eos"} {break}
                if {[dict get $e type] == "err"} {return $e}
                if {[dict get $e type] == "clo"} {
                    return [make_element bra $s $t]
                }
            }
            set error_message "unterminated braces \["
            set error_pos $s
            return [make_element err "$error_message $error_pos" ""]
        }
        ")" {
            set indent false
            return [make_element clo $s $t]
        }
        "\}" {
            set indent false
            return [make_element clo $s $t]
        }
        "\]" {
            set indent false
            return [make_element clo $s $t]
        }
        default {
            if {$c == "\0"} {write_string "not eos, at least one char in token"}
            set indent false
            while {[string length $t] > 0} {
                switch -- [string index $t 0] {
                    " " - "\t" - "\n" - ";" - "=" - "/" - "\\" - "\"" - "'" - "(" - "\{" - "\[" - ")" - "\}" - "\]"  {
                        return [make_element tok $s $t]
                    }
                }
                set t [string range $t 1 end]
            }
            return [make_element tok $s $t]
        }
    }
}

proc get_elements {filename source_code} {
    variable allElements
    append source_code "\0"
    set elements [list]
    set e [scan_next $source_code]
    while {[dict get $e "type"] ne "eos"} {
        if {[dict get $e "type"] eq "err"} {
            error "[file tail $filename]: $e"
        }
        if {[llength $elements]>0} {
            set ex [lindex $elements end]
            #debug "setting next in $ex as [llength $elements]"
            set uid [dict get $e uid]
            dict set ex next $uid
            #dict set ex end "";# we don't need this anymore and it just clutters memory
            set uid [dict get $ex uid]
            dict set allElements $uid $ex
            lset elements end $ex
        }
        set newsource [dict get $e "end"]
        dict set e "end" ""
        lappend elements $e
        set e [scan_next $newsource]
    }
    
    return $elements
}

proc is_asg {e} {
    return [expr {$e ne "" && [dict get $e type] eq "asg"}]
}

proc is_curly {e} {
    return [expr {$e ne "" && [dict get $e type] eq "cur"}]
}

proc is_struct_union_enum {e} {
    if {$e eq "" || [dict get $e type] ne "tok"} {
        return false
    }
    set s [dict get $e begin]
    return [expr {[cstring_equal $s "struct"] || [cstring_equal $s "union"] || [cstring_equal $s "enum"]}]
}

proc is_ext {e} {
    if {$e eq "" || [dict get $e type] ne "tok"} {
        return false
    }
    set s [dict get $e begin]
    return [cstring_equal $s "extern"]
}

proc is_extC {e} {
    if {$e eq "" || [dict get $e type] ne "tok"} {
        return false
    }
    set s [dict get $e begin]
    if {[cstring_equal $s "\"C\""]} {
        dict set e "type" "exc"
        return true
    }
    return false
}

proc is_typedef {e} {
    if {$e eq "" || [dict get $e type] ne "tok"} {
        return false
    }
    set s [dict get $e begin]
    return [cstring_equal $s "typedef"]
}

proc skip_whi_lbr {e} {
    while {$e ne ""} {
        if {[dict get $e type] ne "whi" && [dict get $e type] ne "lbr"} {
            return $e
        }
        # todo get_elementfromlist by number
        set e [get_element_by_uid [dict get $e next]]
    }
    return ""
}

proc skip_whi_lbr_sem {e} {
    while {$e ne ""} {
        if {[dict get $e type] ne "whi" && [dict get $e type] ne "lbr" && [dict get $e type] ne "sem"} {
            return $e
        }
        # todo get_elementfromlist by number
        set e [get_element_by_uid [dict get $e next]]
    }
    return ""
}

proc createPhrase {type is_public first last} {
    return [list $type $is_public $first $last]
}

proc setphrasetype {statevar value} {
    upvar $statevar state
    set phrase [dict get $state phrase]
    lset phrase 0 $value
    #debug "phrase $phrase"
    dict set state phrase $phrase
    #debug "state $state"
}

proc get_phrase {elementList} {
    set state [list input $elementList phrase [createPhrase "unknown" false $elementList $elementList]]
    # skip initial whitespace
    set state [lreplace $state 1 1 [skip_whi_lbr [lindex $state 1]]]
    f_start state
    set phrase [dict get $state phrase]
    #debug "phrase $phrase\n--> $state\n"
    lset phrase 3 [dict get $state input]
    # debug
    #set uid_first [dict get [lindex $phrase 2] uid] 
    #set uid_last [dict get [lindex $phrase 3] uid]
    #debug "from $uid_first to $uid_last"
    #debug "state $state"
    #state.phrase.last = state.input;
    return $phrase
}

proc print_phrase {phrase} {
    set phrase_text ""
    set phrase_elements {}
    if {[lindex $phrase 0] ne "extC"} {
        append phrase_text "phr:\[[lindex $phrase 0]: "
        set e [lindex $phrase 2]
        set f [lindex $phrase 3]
        if {$e ne "" && $f ne ""} {
            set uid_first [dict get $e uid] 
            set uid_last [dict get $f uid]
            set n [dict get $e next]
            #debug "...from $uid_first to $uid_last"
            set phr_str ""
            while {($e!="")&&($n!="")} {
                set p [dict get $e begin]
                if {[dict get $e type]=="tok"} {
                    # keep this for later use as func name
                    set lasttok $p
                }
                #add element
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
                set e [get_element_by_uid $n]
                    
            }
            append phrase_text [string trim $phr_str]
            
        }
        append phrase_text "]\n"
        if {[lindex $phrase 0] eq "fun_dec" || [lindex $phrase 0]  eq "fun_def"} {
            append phrase_text "fun: $lasttok\n"
        }
        foreach e $phrase_elements {
            set type [dict get $e type]
            set txt [dict get $e begin]
            append phrase_text "$type:$txt\n"
        }
    } else {
        append phrase_text "phr:\[extern C suppressed\]\n"
    }
    #debug $phrase_elements 
    write_string $phrase_text
    return
}

proc print_phrases {e} {
    while {$e != ""} {
        set e [skip_whi_lbr_sem $e]
        if {$e == ""} {break}
        set phrase [get_phrase $e]
        print_phrase $phrase
        set e [lindex $phrase 3]
        if {$e == ""} break;
        set e [get_element_by_uid [dict get $e next]]
    }
}

# Function to get the next state
proc next {statevar} {
    #debug "next statevar $statevar"
    upvar $statevar state
    #debug "--> $state"
    set element [dict get $state input]
    if {$element eq ""} {
        return $statevar
    }
    set element [get_element_by_uid [dict get $element next]]
    set element [skip_whi_lbr $element]
    if {$element==""} {
        write_string "exit here"
    }
    dict set state input $element
    # write_string "returning $state"
    # update
    return $statevar
}

# Function to get the symbol type
proc symbol {state} {
    set element [dict get $state input]
    if {$element eq ""} {
        return eos
    }
    return [dict get $element type]
}

# Function for extern C handling
proc f_extc {_state} {
    variable allElements
    upvar $_state state
    #debug [dict get [dict get $state input] begin]
    set length [string length [dict get [dict get $state input] end]]
    set char [string index [dict get [dict get $state input] begin] end]
    if {$char ne "\}" } {;#\{
        write_string "# Error: Closing bracket expected, got $char"
        return
    }
    # parse the hidden source
    set newelem [dict get [dict get $state input] begin]
    set newsource [string range $newelem 1 end-1] 
    set nel [get_elements "externC" $newsource]
    
    # link to existing nodes
    set el [dict get $state input next]
    set el_last [lindex $nel end]
    dict set el_last next $el
    
    # push to allElements
    set uid [dict get $el_last uid]
    dict set allElements $uid $el_last
    
    #update state
    dict set state input [lindex $nel 0]
}

# Function to start processing
proc f_start {_state} {
    upvar $_state state
    if {[dict get $state input]==""} {
        error "exit"
        return 
    }
    #debug "f_start switch [symbol $state]"
    switch [symbol $state] {
        exc {
            # no operation
        }
        tok {
            set element [dict get $state input]
            if {[is_struct_union_enum $element]} {
                f_struct_union_enum [next state]
                #next state
            } elseif {[is_typedef $element]} {
                f_typedef [next state]
            } elseif {[is_ext $element]} {
                next state
                if {[is_extC [dict get $state input]]} {
                    next state
                    setphrasetype state extC
                    dict set state input type exc
                    f_extc state
                }
            } else {
                f_tok [next state]
            }
        }
        pub {f_pub [next state]}
        pre {f_pre state}
        lco {f_lco state}
        bco {f_bco state}
        default {f_err state}
    }
}

proc f_pub {_state} {
    upvar $_state state
    #debug "f_pub switch [symbol $state]"
    dict set [dict get $state phrase] is_public true
    switch [symbol $state] {
        tok {
            set element [dict get $state input]
            if {[is_struct_union_enum $element]} {
                f_struct_union_enum [next state]
            } elseif {[is_typedef $element]} {
                f_typedef [next state]
            } else {
                f_tok [next state]
            }
        }
        pre {f_pre state}
        lco {f_lco state}
        bco {f_bco state}
        default {f_err state}
    }
}

proc f_tok {_state} {
    upvar $_state state
    #debug "f_tok switch [symbol $state] [dict get $state input]"
    if {[dict get $state input]==""} {
        write_string "exit"
        return 
    }
    switch [symbol $state] {
        tok {f_tok [next state]}
        sem {f_tok_sem state}
        par {f_tok_paren [next state]}
        bra {f_tok_bracket [next state]}
        asg {f_tok_asg [next state]}
        lco {f_tok [next state]}
        bco {f_tok [next state]}
        default {f_err state}
    }
}

proc f_tok_paren {_state} {
    upvar $_state state
    #debug "f_tok_paren switch [symbol $state]"
    switch [symbol $state] {
        sem {f_tok_paren_sem state}
        cur {f_tok_paren_curly state}
        lco {f_tok_paren [next state]}
        bco {f_tok_paren [next state]}
        tok {f_tok [next state]}
        default {f_err state}
    }
}

proc f_tok_paren_sem {_state} {
    upvar $_state state
    
    setphrasetype state fun_dec
}

proc f_tok_paren_curly {_state} {
    upvar $_state state

    setphrasetype state fun_def
}

proc f_tok_sem {_state} {
    upvar $_state state

    setphrasetype state var_dec
}

proc f_tok_asg {_state} {
    upvar $_state state

    #debug "f_tok_asg switch [symbol $state]"
    switch [symbol $state] {
        sem {f_tok_asg_sem state}
        default {f_tok_asg [next state]}
    }
}

proc f_tok_asg_sem {_state} {
    upvar $_state state

    setphrasetype state var_def
}

proc f_tok_bracket {_state} {
    upvar $_state state

    #debug "f_tok_bracket switch [symbol $state]"
    switch [symbol $state] {
        sem {f_tok_bracket_sem state}
        bra {f_tok_bracket [next state]}
        asg {f_tok_bracket_asg [next state]}
        lco {f_tok_bracket [next state]}
        bco {f_tok_bracket [next state]}
        default {f_err state}
    }
}

proc f_tok_bracket_sem {_state} {
    upvar $_state state

    setphrasetype state arr_dec
}

proc f_tok_bracket_asg {_state} {
    upvar $_state state

    #debug "f_tok_bracket_asg switch [symbol $state]"
    switch [symbol $state] {
        sem {f_tok_bracket_asg_sem state}
        default {f_tok_bracket_asg [next state]}
    }
}

proc f_tok_bracket_asg_sem {_state} {
    upvar $_state state

    setphrasetype state arr_def
}

proc f_struct_union_enum {_state} {
    upvar $_state state

    #debug "f_struct_union_enum switch [symbol $state]"
    switch [symbol $state] {
        sem {f_struct_union_enum_sem state}
        default {f_struct_union_enum [next state]}
    }
}

proc f_struct_union_enum_sem {_state} {
    upvar $_state state

    setphrasetype state struct_union_enum_def
}

proc f_typedef {_state} {
    upvar $_state state

    #debug "f_typedef switch [symbol $state]"
    switch [symbol $state] {
        sem {f_typedef_sem state}
        default {f_typedef [next state]}
    }
}

proc f_typedef_sem {_state} {
    upvar $_state state

    setphrasetype state type_def
}

proc f_pre {_state} {
    upvar $_state state

    setphrasetype state preproc
}

proc f_lco {_state} {
    upvar $_state state

    setphrasetype state line_comment
}

proc f_bco {_state} {
    upvar $_state state

    setphrasetype state block_comment
}

proc f_err {_state} {
    upvar $_state state

    #debug "found error in phrase $state"
    #debug [dict get state phrase]
    #setphrasetype state error
}

proc selftest {} {
    # test 
    #init
    ctokenizer::init
    
    set s "int f (int a);\n//comment\n\n int f2 ();\n\n"
    set el [ctokenizer::get_elements "" $s]
    foreach e $el {
        set type [dict get $e type]
        set what [dict get $e begin]
        set next [dict get $e next]
        #debug "$type: $what $next"
        #debug "next: $next [get_element_by_uid $next]"
    }
    #debug $allElements
    set e [dict get $ctokenizer::allElements 0]
    ctokenizer::print_phrases $e
    return
}

}


# usage
#ctokenizer::parse_file [lindex $argv 0] "putfile" "test2.defs"
#set ctokenizer::writestring ""
# or
#ctokenizer::parse_file $fname "stringify"; set ts $ctokenizer::writestring

