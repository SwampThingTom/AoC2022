#!/usr/bin/env tclsh

# Monkey in the Middle
# https://adventofcode.com/2022/day/11

proc debugPrint {msg} {
    # set to 1 to show debug output
    if { 0 } {
        puts $msg
    }
}

proc readInput {} {
    set f [open "input.txt" r]
    set lines [split [string trim [read $f]] "\n"]
    close $f
    return $lines
}

proc showMonkey {monkey} {
    upvar $monkey m
    debugPrint [dict get $m items]
    debugPrint [dict get $m operation]
    debugPrint [dict get $m test]
    debugPrint [dict get $m ifTrue]
    debugPrint [dict get $m ifFalse]
    debugPrint [dict get $m inspected]
}

proc makeMonkey {items operation test ifTrue ifFalse} {
    return [dict create items $items operation $operation test $test ifTrue $ifTrue ifFalse $ifFalse inspected 0]
}

proc throwTo {target item} {
    upvar $target t
    set newItems [dict get $t items]
    lappend newItems $item
    dict set t items $newItems
    return $t
}

proc solve {count reduceWorry} {
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
            }
            2 {
                set operation [lrange $tokens 5 end]
                lappend monkeyArgs [string map {old "$old"} $operation]
            }
            3 {
                set test [lindex $tokens end]
                lappend monkeyArgs $test
            }
            4 {
                set ifTrue [lindex $tokens end]
                lappend monkeyArgs $ifTrue
            }
            5 {
                set ifFalse [lindex $tokens end]
                lappend monkeyArgs $ifFalse
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

    # Calculate gcf of test values.
    # (Only used for part 2.)
    set gcf 1
    dict for {index monkey} $monkeys {
        set gcf [expr $gcf * [dict get $monkey test]]
    }

    # Run for count rounds.
    for {set i 0} {$i < $count} {incr i} {
        foreach index [dict keys $monkeys] {
            set monkey [dict get $monkeys $index]
            debugPrint "Monkey #$index"

            # Take turn for monkey.
            set items [dict get $monkey items]
            set inspected [dict get $monkey inspected]
            foreach old $items {
                debugPrint "  Inspect item with worry level of $old."
                set inspected [expr $inspected + 1]
            
                set new [expr [dict get $monkey operation]]
                debugPrint "    New worry level is $new."

                set new [expr $reduceWorry]
                debugPrint "    Monkey bored, worry level is now $new."
                
                if { [expr $new % [dict get $monkey test]] == 0 } {
                    set targetIndex [dict get $monkey ifTrue]
                    debugPrint "    Throwing to monkey $targetIndex"

                    set target [dict get $monkeys $targetIndex]
                    dict set monkeys $targetIndex [throwTo target $new]
                } else {
                    set targetIndex [dict get $monkey ifFalse]
                    debugPrint "    Throwing to monkey $targetIndex"
                    
                    set target [dict get $monkeys $targetIndex]
                    dict set monkeys $targetIndex [throwTo target $new]
                }
            }

            dict set monkey inspected $inspected
            dict set monkey items {}
            dict set monkeys $index $monkey
            debugPrint ""
        }

        dict for {index monkey} $monkeys {
            set items [dict get $monkey items]
            set inspected [dict get $monkey inspected]
            debugPrint "Monkey #$index ($inspected): $items"
        }
        debugPrint ""
    }

    set max1 0
    set max2 0
    dict for {index monkey} $monkeys {
        set inspected [dict get $monkey inspected]
        if { $inspected > $max1 } {
            set max2 $max1
            set max1 $inspected
        } elseif { $inspected > $max2 } {
            set max2 $inspected
        }
    }

    return [expr $max1 * $max2]
}

set part1 [solve 20 "\$new / 3"]
puts "Part 1: $part1"

set part2 [solve 10000 "\$new % \$gcf"]
puts "Part 2: $part2"