// Monkey Math
// https://adventofcode.com/2022/day/21

import java.io.File
import java.util.ArrayDeque

typealias MonkeyMap = Map<String, List<String>>
typealias MutableValuesMap = MutableMap<String, Long>

fun debugPrint(msg: String, show: Boolean = false) = if (show) { println(msg) } else {}

fun readInput(): List<String> = File("input.txt").useLines { it.toList() }

fun parseMonkey(line: String): Pair<String, List<String>> {
    val components = line.split(": ")
    val value = components[1].split(" ")
    return components[0] to value
}

fun solvePart1(monkeys: MonkeyMap): Long {
    val values: MutableValuesMap = mutableMapOf()
    return solveFor("root", monkeys, values)
}

fun solvePart2(monkeys: MonkeyMap): Long {
    val values: MutableValuesMap = mutableMapOf()
    val rootExpr = monkeys["root"]!!

    val (humnMonkey, otherMonkey) = whichMonkeyLeadsToHumn(rootExpr[0], rootExpr[2], monkeys)
    val expectedValue = solveFor(otherMonkey, monkeys, values)
    return solveForHumn(humnMonkey, expectedValue, monkeys, values)
}

fun whichMonkeyLeadsToHumn(lhs: String, rhs: String, monkeys: MonkeyMap): Pair<String, String> =
    if (leadsToHumn(lhs, monkeys)) Pair(lhs, rhs) else Pair(rhs, lhs)

fun leadsToHumn(monkey: String, monkeys: MonkeyMap): Boolean {
    if (monkey == "humn") return true
    
    val expr = monkeys[monkey]!!
    if (expr.size == 1) return false

    return leadsToHumn(expr[0], monkeys) || leadsToHumn(expr[2], monkeys)
}

fun solveForHumn(monkey: String, expectedValue: Long, monkeys: MonkeyMap, values: MutableValuesMap): Long {
    if (monkey == "humn") {
        return expectedValue
    }
    
    val expr = monkeys[monkey]!!
    assert(expr.size == 3)

    val (humnMonkey, otherMonkey) = whichMonkeyLeadsToHumn(expr[0], expr[2], monkeys)
    val knownValue = solveFor(otherMonkey, monkeys, values)
    val newExpectedValue = evaluateInverse(expr[1], expectedValue, knownValue, humnMonkey == expr[0])
    
    return solveForHumn(humnMonkey, newExpectedValue, monkeys, values)
}

fun solveFor(monkey: String, monkeys: MonkeyMap, values: MutableValuesMap): Long {
    val value = values[monkey]
    if (value is Long) {
        return value
    }

    val expr = monkeys[monkey]!!
    if (expr.size == 1) {
        values[monkey] = expr[0].toLong()
        return values[monkey]!!
    }
    
    assert(expr.size == 3)
    val lhs = values[expr[0]] ?: solveFor(expr[0], monkeys, values)
    val rhs = values[expr[2]] ?: solveFor(expr[2], monkeys, values)
    values[monkey] = evaluate(expr[1], lhs, rhs)
    return values[monkey]!!
}

fun evaluate(op: String, lhs: Long, rhs: Long): Long {
    return when (op) {
        "+" -> lhs + rhs
        "-" -> lhs - rhs
        "*" -> lhs * rhs
        "/" -> lhs / rhs
        else -> throw IllegalArgumentException("Unexpected operator.")
    }
}

fun evaluateInverse(op: String, lhs: Long, rhs: Long, lhsIsHumn: Boolean): Long {
    return when (op) {
        "+" -> lhs - rhs
        "-" -> if (lhsIsHumn) lhs + rhs else rhs - lhs
        "*" -> lhs / rhs
        "/" -> lhs * rhs
        else -> throw IllegalArgumentException("Unexpected operator.")
    }
}

fun main() {
    val input = readInput()
    val monkeys: MonkeyMap = input.map { parseMonkey(it) }.toMap()

    val part1 = solvePart1(monkeys)
    println("Part 1: $part1")

    val part2 = solvePart2(monkeys)
    println("Part 2: $part2")
}
