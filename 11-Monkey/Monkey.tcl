#!/usr/bin/env tclsh

# Monkey in the Middle
# https://adventofcode.com/2022/day/11

proc readInput {} {
    set f [open "input.txt" r]
    set lines [split [string trim [read $f]] "\n"]
    close $f
    return $lines
}

proc showMonkey {monkey} {
    upvar $monkey m
    puts [dict get $m items]
    puts [dict get $m operation]
    puts [dict get $m test]
    puts [dict get $m ifTrue]
    puts [dict get $m ifFalse]
}

proc makeMonkey {items operation test ifTrue ifFalse} {
    return [dict create items $items operation $operation test $test ifTrue $ifTrue ifFalse $ifFalse]
}

# dict set monkeys 0 [makeMonkey [list 79 98] [list * old 19] 23 2 3]
# dict set monkeys 1 [makeMonkey [list 54 65 75 74] [list + old 6] 19 2 0]

# for {set i 0} {$i < [dict size $monkeys]} {incr i} {
#     puts "Monkey #$i"
#     set monkey [dict get $monkeys $i]
#     showMonkey monkey
# }

# Parse input into monkey dictionaries.
set i 0
set monkeyArgs [list]
set monkeyIndex 0
foreach line [readInput] {
    set tokens [split $line]
    switch $i {
        1 {
            set itemsStr [regsub -all { } [join [lrange $tokens 4 end]] ""]
            set items [split $itemsStr ","]
            lappend monkeyArgs $items
            # puts "  Items: $items"
          }
        2 {
            set operation [lrange $tokens 5 end]
            lappend monkeyArgs $operation
            # puts "  Operation: $operation"
          }
        3 {
            set test [lindex $tokens end]
            lappend monkeyArgs $test
            # puts "  Test: $test"
          }
        4 {
            set ifTrue [lindex $tokens end]
            lappend monkeyArgs $ifTrue
            # puts "  If True: $ifTrue"
          }
        5 {
            set ifFalse [lindex $tokens end]
            lappend monkeyArgs $ifFalse
            # puts "  If False: $ifFalse"
          }
    }
    set i [expr $i + 1]
    if {$i == 6} {
        set monkey [makeMonkey {*}$monkeyArgs]
        dict set monkeys $monkeyIndex $monkey
        set monkeyIndex [expr $monkeyIndex + 1]
 
    }
    if {$i == 7} {
        set i 0
        set monkeyArgs [list]
    }
}

dict for {index monkey} $monkeys {
    puts "Monkey #$index"
    showMonkey monkey

    # TODO: Process a monkey
}