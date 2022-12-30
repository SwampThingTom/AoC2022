#!/usr/bin/env -S deno run --allow-read --allow-write

// Grove Positioning System
// https://adventofcode.com/2022/day/20

const DEBUG: boolean = false
const debugLog = ((text: string) => { if (DEBUG) console.log(text) })

function assert(condition: any, msg?: string): asserts condition {
    if (!condition) {
        throw new Error(msg)
    }
}

class ListNode {
    public value: number
    public next: ListNode | null
    public prev: ListNode | null

    constructor(value: number) {
        this.value = value
        this.next = null
        this.prev = null
    }
}

class List {
    private head: ListNode | null
    private size: number
 
    static fromArray(ary: number[]): List {
        const list = new List()
        ary.forEach(item => list.add(item))
        return list
    }

    constructor() {
        this.head = null
        this.size = 0
    }
 
    public length(): number {
        return this.size
    }
 
    public isEmpty(): boolean {
        return this.size == 0
    }

    public toArray(): number[] {
        if (this.isEmpty()) {
            return []
        }
        let ary: number[] = []
        let cursor: ListNode = this.head!
        do {
            ary.push(cursor.value)
            cursor = cursor.next!
        } while (cursor != this.head)
        return ary
    }

    [Symbol.iterator]() {
        let cursor = this.head
        let index = 0
        return {
            next: () => {
                if (index >= this.size) {
                    return { value: undefined, done: true }
                }
                const value = cursor!.value
                cursor = cursor!.next
                index++
                return { value: value, done: false }
            }
        }
      }

    public add(value: number): ListNode {
        let node = new ListNode(value)
        if (this.isEmpty()) {
            node.next = node
            node.prev = node
            this.head = node
            this.size++
        } else {
            let tail = this.head!.prev
            node.next = this.head
            node.prev = tail
            tail!.next = node
            this.head!.prev = node
            this.size++
        }
        return node
    }

    public moveNode(node: ListNode) {
        let count = node.value % (this.length() - 1)

        let target = node
        if (node.value > 0) {
            target = this.findNodeAfter(node, count)
        } else if (node.value < 0) {
            target = this.findNodeBefore(node, count)
        }

        if (target == node) {
            return
        }

        if (this.head == node) {
            this.head = node.next
        }
        
        // Remove node from previous position.
        node.prev!.next = node.next
        node.next!.prev = node.prev

        // Insert after target.
        node.next = target.next
        node.prev = target
        node.prev!.next = node
        node.next!.prev = node
    }

    public getResult(): number {
        let node = this.find(0)
        assert(node, `Value 0 not found in list.`)

        let result = 0
        for (let i = 0; i < 3; i++) {
            node = this.findNodeAfter(node, 1_000)
            result += node.value
        }
        return result
    }

    private find(value: number): ListNode | null {
        let node = this.head
        if (!node) return null
        do { 
            if (node!.value == value) {
                return node
            }
            node = node!.next
        } while (node != this.head)
        return null
    }

    private findNodeAfter(node: ListNode, count: number): ListNode {
        assert(count>0, "Count must be greater than 0.")
        let target: ListNode = node
        while (count-- > 0) {
            target = target.next!
        }
        return target
    }

    private findNodeBefore(node: ListNode, count: number): ListNode {
        assert(count<0, "Count must be less than 0.")
        let target: ListNode = node
        while (count++ <= 0) {
            target = target.prev!
        }
        return target
    }
}

function printList(list: List) {
    debugLog(`${list.toArray().toString()}`)
}

function solve(encrypted: number[], mixCount: number = 1): number {
    assert(mixCount > 0, "Mix count must be a positive integer.")

    const list = new List()
    let nodes: ListNode[] = []
    for (const value of encrypted) {
        nodes.push(list.add(value))
    }
    printList(list)
    assert(list.toArray().length == encrypted.length, "Array lengths don't match at start.")
    
    while (mixCount-- > 0) {
        for (const node of nodes) {
            list.moveNode(node)
            debugLog(`Moved ${node.value}`)
        }
    }
    assert(list.toArray().length == encrypted.length, "Array lengths don't match after mixing.")
    printList(list)

    return list.getResult()
}

const encrypted: number[] = Deno
    .readTextFileSync('input.txt')
    .split(("\n"))
    .filter((x: string) => x)
    .map((x: string) => parseInt(x))

const part1 = solve(encrypted)
console.log(`Part 1: ${part1}`)

const decrypted = encrypted.map( x => x * 811589153 )
const part2 = solve(decrypted, 10)
console.log(`Part 2: ${part2}`)
