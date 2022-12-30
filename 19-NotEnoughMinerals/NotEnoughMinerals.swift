#!/usr/bin/swift

// Not Enough Minerals
// https://adventofcode.com/2022/day/19

import Foundation

// An array of tuples where the first element is an amount and the second is a resource.
typealias Costs = [(Int, String)]

// The resource costs for a robot that collects a particular resource.
struct Robot {
    let collects: String
    let costs: Costs
}

// Represents a factory that is capable of building robots that collect resources.
struct Factory {
    let blueprint: [String: Costs]
    var robots: [String: Int]
    var resources: [String: Int]

    let maxMinutes: Int
    var minute = 1

    var isFinished: Bool { minute > maxMinutes }
    var resourceTypes: [String] { Array(blueprint.keys) }
    var geodes: Int { resources["geode"]! }

    init(_ blueprint: [Robot], minutesToRun: Int) {
        self.blueprint = Dictionary(uniqueKeysWithValues: blueprint.map { ($0.collects, $0.costs) })
        self.robots = Dictionary(uniqueKeysWithValues: blueprint.map { ($0.collects, 0) })
        self.resources = Dictionary(uniqueKeysWithValues: blueprint.map { ($0.collects, 0) })
        self.robots["ore"] = 1
        self.maxMinutes = minutesToRun
    }

    // Returns a new factory that represents the state after building the requested robot.
    // The caller must first have verified that the robot can be built by calling `canBuild()`.
    func factory(afterBuilding newRobot: String) -> Factory {
        assert(minute <= maxMinutes)
        var newFactory = self
        var robotBuilt = false
        while !newFactory.isFinished && !robotBuilt {
            robotBuilt = newFactory.runOneMinute(building: newRobot)
        }
        return newFactory
    }

    // Runs the factory for one minute, building the given robot if possible.
    // Returns true if the robot was built.
    private mutating func runOneMinute(building robot: String) -> Bool {
        var didBuildRobot = false
        debugPrint("== Minute \(minute) ==")
        
        if canBuildNow(robot) {
            startBuilding(robot)
            didBuildRobot = true
        }
        
        collectResources()
        
        if didBuildRobot {
            build(robot)
        }
        
        minute += 1
        return didBuildRobot
    }

    // Returns true if we have robots available that can collect the resources
    // needed to build the given robot.
    func canBuild(_ robot: String) -> Bool {
        return blueprint[robot]!.allSatisfy { robots[$0.1]! > 0 }
    }

    // Returns true if we have the resources to build a robot of the given type.
    private func canBuildNow(_ robot: String) -> Bool {
        return blueprint[robot]!.allSatisfy { resources[$0.1]! >= $0.0 }
    }

    // Heuristic to determine whether a robot is worth building came from this reddit post.
    // https://www.reddit.com/r/adventofcode/comments/zpy5rm/2022_day_19_what_are_your_insights_and/
    func shouldBuild(_ robot: String) -> Bool {
        if robot == "geode" { return true }
        let numRobots = robots[robot]!
        let numResource = resources[robot]!
        let minutesRemaining = maxMinutes - minute + 1
        let maxNeeded = maxNeeded(resource: robot)
        return numRobots * minutesRemaining + numResource < minutesRemaining * maxNeeded 
    }

    // Returns the maximum amount of the given resource required to build any robot.
    private func maxNeeded(resource: String) -> Int {
        blueprint.values.map { costs in (costs.first { $0.1 == resource }?.0) ?? 0 }.max()!
    }

    // Deducts the resources used to build the given robot (but does not yet add the robot to the factory).
    private mutating func startBuilding(_ robot: String) {
        debugPrint("Spend \(blueprint[robot]!) to start building a \(robot)-collecting robot.")
        blueprint[robot]!.forEach {
            resources[$0.1]! -= $0.0
            assert(resources[$0.1]!>=0)
        }
    }

    // Adds a robot of the given type to the factory.
    private mutating func build(_ robot: String) {
        robots[robot] = (robots[robot] ?? 0) + 1
        debugPrint("The new \(robot)-collecting robot is now ready; you now have \(robots[robot]!) of them.")
    }

    // Adds the resources collected by all robots in the factory for one minute.
    private mutating func collectResources() {
        robots.forEach { res, numRobots in
            guard numRobots > 0 else { return }
            resources[res] = (resources[res] ?? 0) + numRobots 
            debugPrint("\(numRobots) \(res)-collecting robot collects \(numRobots) \(res). You now have \(resources[res]!) \(res).")
        }
    }
}

// Returns the maximum number of geodes that the factory can collect.
func run(factory: Factory) -> Int {
    if factory.isFinished {
        return factory.geodes
    }

    var maxGeodes = 0
    for robot in factory.resourceTypes {
        if factory.canBuild(robot) && factory.shouldBuild(robot) {
            let newFactory = factory.factory(afterBuilding: robot)
            let geodes = run(factory: newFactory)
            if geodes > maxGeodes {
                maxGeodes = geodes
            }
        }
    }

    return maxGeodes
}

// Part 1
// Returns the quality of the given blueprint.
// Quality is the number of geodes it provides in 24 minutes times its blueprint ID.
func qualityOf(blueprint: [Robot], withId blueprintId: Int) -> Int {
    let geodes = run(factory: Factory(blueprint, minutesToRun: 24))
    return geodes * blueprintId
}

// Part 2
// Returns the number of geodes a blueprint can provide in 32 minutes.
func geodesCollectedBy(blueprint: [Robot]) -> Int {
    return run(factory: Factory(blueprint, minutesToRun: 32))
}

func debugPrint(_ message: String, showDebug: Bool = false) {
    if showDebug { print(message) }
}

func readInput(named name: String = "input.txt") -> [String] {
    let currentDirectoryURL = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
    let fileURL = URL(fileURLWithPath: name, relativeTo: currentDirectoryURL)
    guard let content = try? String(contentsOf: fileURL, encoding: String.Encoding.utf8) else {
        print("Unable to read input file \(name)")
        print("Current directory: \(currentDirectoryURL)")
        return []
    }
    return content.components(separatedBy: .newlines).filter { $0 != "" }
}

func parse(_ line: String) -> [Robot] {
    return line.split(separator: ":")[1].split(separator: ".").map { parseRobot(String($0)) }
}

func parseRobot(_ str: String) -> Robot {
    let words = str.split(separator: " ")
    let costs = stride(from: 4, to: words.count, by: 3).map { 
        (Int(words[$0])!, String(words[$0 + 1]))
    }
    return Robot(collects: String(words[1]), costs: costs)
}

let blueprints = readInput().map(parse)

let sumOfBlueprintQualities = blueprints.enumerated().map { qualityOf(blueprint: $1, withId: $0 + 1) }.reduce(0, +)
print("Part 1: \(sumOfBlueprintQualities)")

let productOfGeodes = blueprints[0...2].map { geodesCollectedBy(blueprint: $0) }.reduce(1, *)
print("Part 2: \(productOfGeodes)")
