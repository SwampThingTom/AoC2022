import scala.math._

object ProboscideaVolcanium {

    case class Valve(flow: Int, neighbors: Seq[String])
    type ValveMap = Map[String, Valve]
    type Paths = Map[String, Map[String, Seq[String]]]
    type State = (Seq[String], Seq[Int], Set[String])

    val ValveRegEx = raw".*([A-Z]{2}).*=(\d+).*valves? (.*)".r
    def parseValve(str: String): (String, Valve) =
        str match {
            case ValveRegEx(valve, flow, neighbors) => {
                valve -> Valve(flow.toInt, neighbors.split(",").map(_.trim).toList)
            }
        }
    
    def parseInput(): ValveMap = {
        val bufferedSource = io.Source.fromFile("input.txt")
        val valves = bufferedSource
            .getLines()
            .map(parseValve)
            .toMap
        bufferedSource.close
        valves
    }

    // Floyd-Warshall algorithm
    // https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
    def findShortestPaths(valves: ValveMap): Paths = {
        val paths: collection.mutable.Map[String, collection.mutable.Map[String, List[String]]] = collection.mutable.Map()
        for ((name, valve) <- valves) {
            paths(name) = collection.mutable.Map(name -> List[String]())
            for (neighbor <- valve.neighbors) {
                paths(name)(neighbor) = List[String](neighbor)
            }
        }
        for (k <- valves.keys) {
            for (i <- valves.keys) {
                paths(i).get(k).map { ik =>
                    for (j <- valves.keys) {
                        paths(k).get(j).map { kj =>
                            val ijLength = paths(i).get(j).fold(100)(_.length)
                            if (ijLength > ik.length + kj.length) {
                                paths(i)(j) = ik ::: kj
                            }
                        }
                    }
                }
           }
        }
        paths.map((k,v) => k -> v.toMap).toMap
    }

    def findMaxPressureReleased(valves: ValveMap, paths: Paths): Int = {
        val priority = valves.keys.filter(valves(_).flow > 0).toSeq.sortWith(valves(_).flow > valves(_).flow)
        val states: Seq[State] = List((List[String](), List[Int](), Set[String]()))
        openValves(valves, paths, priority, states, 0)
    }

    def openValves(valves: ValveMap, paths: Paths, priority: Seq[String], states: Seq[State], maxPressureReleased: Int): Int = {
        if (states.isEmpty) {
            return maxPressureReleased
        }
        
        val newStates = collection.mutable.ListBuffer[State]()
        newStates ++= states.drop(1)

        val (path, pressures, opened) = states.head
        val valve = path.lastOption.getOrElse("AA")
        val currentlyReleasing = pressures.lastOption.getOrElse(0)
        val openList = priority.filter(!opened.contains(_))

        for (next <- openList) {
            val pathToNext = paths(valve)(next)
            if (path.length + pathToNext.length + 1 < 30) {
                // Each element in new path represents a valve visited + the valve being opened.
                // Thus the valve being opened will show up twice in a row.
                val newPath = path.toList ::: pathToNext.toList ::: List(next)
                // New pressures is parallel to new path and contains the amount of pressure released each minute.
                // Extending the list to 30 items and summing the result provides the total pressure released for the path.
                val pathPressure = List.fill(pathToNext.length)(currentlyReleasing)
                val newPressures = pressures.toList ::: pathPressure ::: List(valves(next).flow + currentlyReleasing)
                val newState: State = (newPath, newPressures, opened + next)
                newStates += newState
            }
        }

        val totalReleased = sumReleasedPressure(pressures, currentlyReleasing)
        openValves(valves, paths, priority, newStates.toList, max(maxPressureReleased, totalReleased))
    }

    def sumReleasedPressure(releasedPerTurn: Seq[Int], current: Int): Int = {
        val remainingTurns = if (releasedPerTurn.length < 30) { 30 - releasedPerTurn.length - 1 } else { 0 }
        releasedPerTurn.map((p: Int) => p).sum + remainingTurns * current
    }

    def main(args: Array[String]) = {
        val valves = parseInput()
        val paths = findShortestPaths(valves)

        val part1 = findMaxPressureReleased(valves, paths)
        println(s"Part 1: $part1")
    }
}
