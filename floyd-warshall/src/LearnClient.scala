import java.io.{BufferedReader, InputStreamReader}
import java.util.function.ToIntFunction

import Strategy.AdvancedStrategy
import searchclient.SearchClient.Memory
import searchclient._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * @author miniwolf
  */
object LearnClient extends App {
  def canReachAllRemainingBoxes(goalPos: Position, solvedGoals: List[Position], node: Node): Boolean = {
    Node.walls.add(goalPos)
    (goalPos :: solvedGoals).foreach(goal =>
      node.boxes.filter(box => box.getPosition.equals(goal)).last.setMovable(false))

    val emptyState = new Node(node.parent)
    emptyState.setAgent(node.getAgent)
    val (_, edges) = Graph.construct(emptyState)
    val res = node.boxes.filter(box => box.isMovable) match {
      case list if list.isEmpty => true
      case list => !list.exists(box => Astar.search(edges, node.getAgent.getPosition, box.getPosition).isEmpty)
    }

    Node.walls.remove(goalPos)
    (goalPos :: solvedGoals).foreach(goal =>
      node.boxes.filter(box => box.getPosition.equals(goal)).last.setMovable(true))
    res
  }

  def findSolutionLengths(goalMatches: Map[Position, Int], state: Node): Map[Position, Int] = {
    val emptyStartState = new Node(state.parent)
    emptyStartState.setAgent(state.getAgent)
    val (_, edges) = Graph.construct(emptyStartState)
    goalMatches.map { case (goalPos, _) =>
      val box = state.boxes.find(box => box.getId == goalMatches(goalPos)).get
      val goalPath: List[Position] = box.getGoalPath match {
        case null =>
          val path = Astar.search(edges, goalPos, box.getPosition)
          box.setGoalPath(path)
          path
        case path => path.toList
      }
      val agentPath = Astar.search(edges, state.getAgent.getPosition, box.getPosition)

      goalPos -> (goalPath.length + agentPath.length)
    }
  }

  override def main(args: Array[String]) {
    val serverMessages: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    System.err
      .println("SearchClient initializing. I am sending this using the error output stream.")

    val lines: java.util.List[String] = new java.util.ArrayList[String]
    var line: String = serverMessages.readLine
    while ( !line.equals("") ) {
      lines.add(line)
      line = serverMessages.readLine
    }
    Node.MAX_COLUMN = lines.stream().mapToInt(new ToIntFunction[String] {
      override def applyAsInt(value: String): Int = value.length
    }).max.getAsInt
    Node.MAX_ROW = lines.size
    val client: SearchClient = new SearchClient(lines)
    val startTime = System.currentTimeMillis()
    val learnClient = new LearnClient(client)
    val (_, edges) = Graph.construct(learnClient.emptyStartState)
    val goalMatches: Map[Position, Int] = matchGoalsWithBoxes(learnClient.startState, edges)
    val goalList: mutable.Map[Position, Character] = Node.goals.map { case (goalPos, goalChar) => goalPos->goalChar }

    val solution: List[Node] = findSolution(goalList.keys.toList, goalMatches, List(), List(),
                                            learnClient.startState)
    val timeSpent = (System.currentTimeMillis - startTime) / 1000f
    System.err.println(s"Summary: Time: $timeSpent\t ${Memory.stringRep()}")
    Node.walls.removeAll(goalList.keys.toList)
    goalList.foreach { case (goalPos, goalChar) => Node.goals.put(goalPos, goalChar) }
    solution.foreach { case n: Node =>
      val act = n.action.toActionString
      println(act)
      val response: String = serverMessages.readLine
      if ( response.contains("false") ) {
        System.err.println(s"Server responded with $response to the inapplicable action: $act\n")
        System.err.format(s"$act was attempted in \n")
        return
      }
    }
    //QClient.useInitialPath(client, serverMessages, solution)
  }

  def findDangerousPositions(vertices: List[Position], edges: Map[Position, List[Position]],
                             goalPos: Position, box: Box, safeSpot: Position, empty: Node) = {
    Node.walls.add(goalPos)
    val agentPos = empty.getAgent.getPosition
    empty.getAgent.setPosition(safeSpot)
    val (newVertices, _) = Graph.construct(empty)
    val pathToBox = Astar.search(edges, goalPos, box.getPosition)
    val pathToAgent = Astar.search(edges, agentPos, box.getPosition)
    val diff = vertices.diff(newVertices).diff(pathToBox).diff(pathToAgent)
    Node.walls.remove(goalPos)
    diff.filter(p => !p.equals(goalPos))
  }

  def findSolution(goals: List[Position], goalMatch: Map[Position, Int], solution: List[Node],
                   solvedGoals: List[Position], node: Node): List[Node] = {
    if ( goals.isEmpty ) {
      return solution
    }
    val solutionLength: Map[Position, Int] = findSolutionLengths(goalMatch, node)
    val (dependencies, goalMatches) = getGoalDependencies(goals, goalMatch, node)
    val goalsToSolve = findGoal(dependencies, solutionLength)
    //val fullEstimate = solutionLength.map { case (a,b) => b._1 + b._2 }.sum

    val emptyState = new Node(node.parent)
    emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
    val (_, edges) = Graph.construct(emptyState)
    //val fullEstimate = estimateSolution(node.getAgent.getPosition, goalMatches.toList, 0, node, edges, 0, solutionLength)
    val ignoreGoals = reduceGoalsToSolve(goalsToSolve, Nil, node, goalMatches).sortBy(pos => solutionLength(pos))
    solveBestGoal(goalsToSolve.diff(ignoreGoals), (null, List()), 200000, node, goalMatches, solvedGoals, solutionLength, ignoreGoals) match {
      case (null, List()) =>
        sys.error("Cannot find solution, issue issue!")
      case (goalPos, newSolution: List[Node]) =>
        newSolution.head.parent = solution match {
          case Nil => null
          case _ => solution.last
        }

        val newNode = newSolution.last.ChildNode()
        newNode.parent = null
        Node.walls.add(goalPos)
        Node.goals.remove(goalPos)

        val newGoalMatches = goalMatches.filter { case (pos,_) => !pos.equals(goalPos) }
        val newSolvedGoals: List[Position] = goalPos :: solvedGoals
        val newGoals = goals.filter(goal => !goal.equals(goalPos))
        findSolution(newGoals, newGoalMatches, solution ++ newSolution,
                     newSolvedGoals, newNode)
    }
  }

  def reduceGoalsToSolve(goalsToSolve: List[Position], goalsToIgnore: List[Position], node: Node, goalMatches: Map[Position, Int]): List[Position] = {
    goalsToSolve match {
      case Nil => goalsToIgnore
      case goalPos :: cdr =>
        val emptyState = new Node(node.parent)
        emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
        val (vertices, edges) = Graph.construct(emptyState)
        val boxId = goalMatches.get(goalPos).get
        val box = node.boxes.filter(box => box.getId == boxId).last
        val boxPath = findPath2(node, box, goalPos, edges)
        val agentPath = findPath2(node, box, node.getAgent.getPosition, edges)
        val boxesOnPath = node.boxes.filter(box => boxPath.contains(box.getPosition) ||
                                                   agentPath.contains(box.getPosition))
        if ( boxesOnPath.nonEmpty ) {
          reduceGoalsToSolve(cdr, goalPos :: goalsToIgnore, node, goalMatches)
        } else {
          reduceGoalsToSolve(cdr, goalsToIgnore, node, goalMatches)
        }
    }

  }

  def estimateSolution(goalPos: Position, goalMatches: List[(Position, Int)], estimate: Int,
                       node: Node, edges: Map[Position, List[Position]], knownShortest: Int,
                       solutionLength: Map[Position, (Int, Int)]): Int = {
    goalMatches match {
      case Nil => estimate
      case _ if knownShortest != 0 && estimate > knownShortest => -1
      case (goal, boxId) :: cdr =>
        val box = node.boxes.filter(box => box.getId == boxId).last
        val newEstimate = estimate + Astar.search(edges, goalPos, box.getPosition).size + solutionLength(goal)._1 - 2
        estimateSolution(goalPos, cdr, newEstimate, node, edges, knownShortest, solutionLength)
    }
  }

  def solveBestGoal(goalsToSolve: List[Position], bestSoFar: (Position, List[Node]), threshold: Int,
                    node: Node, goalMatches: Map[Position, Int], solvedGoals: List[Position],
                    solutionLength: Map[Position, Int], goalsWithBoxes: List[Position]): (Position, List[Node]) = {
    goalsToSolve match {
      case Nil if goalsWithBoxes.nonEmpty => solveBestGoal(goalsWithBoxes, bestSoFar, threshold, node, goalMatches, solvedGoals, solutionLength, Nil)
      case Nil => bestSoFar
      case goalPos :: cdr =>
        if ( bestSoFar._2.nonEmpty && solutionLength(goalPos) > bestSoFar._2.length ) {
          solveBestGoal(cdr, bestSoFar, threshold, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
        } else {
          val emptyState = new Node(node.parent)
          emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
          val (vertices, edges) = Graph.construct(emptyState)

          val dangerZone = if ( goalMatches.size != 1 ) {
            val safeBox = node.boxes.filter(box => goalMatches(goalPos) != box.getId && goalMatches.values.contains(box.getId)).last
            val box = node.boxes.find(b => goalMatches(goalPos) == b.getId).get
            findDangerousPositions(vertices, edges, goalPos, box, safeBox.getPosition, emptyState)
          } else {
            List()
          }

          solveReduced(goalPos, goalMatches, dangerZone, solvedGoals, node, edges, threshold) match {
            case null => solveBestGoal(cdr, bestSoFar, bestSoFar._2.length, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
            case newSolution =>
              if ( bestSoFar._2.isEmpty || newSolution.length < bestSoFar._2.length ) {
                solveBestGoal(cdr, (goalPos, newSolution.toList), bestSoFar._2.length, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
              } else {
                solveBestGoal(cdr, bestSoFar, threshold, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
              }
          }
        }
    }
  }

  def findPathWithLimits(path: List[Position], remainingBoxes: List[Box], node: Node, findBox: Box, goal: Position): List[Position] = {
    remainingBoxes match {
      case Nil => path
      case box :: cdr =>
        Node.walls.add(box.getPosition)
        val emptyState = new Node(node.parent)
        emptyState.setAgent(new Agent(node.getAgent.getPosition, 0))
        val (_, edges) = Graph.construct(emptyState)
        Astar.search(edges, findBox.getPosition, goal) match {
          case Nil =>
            Node.walls.remove(box.getPosition)
            findPathWithLimits(path, cdr, node, findBox, goal)
          case newPath => findPathWithLimits(newPath, cdr, node, findBox, goal)
        }
    }
  }

  def findPath(node: Node, ignoreBoxEdges: Map[Position, List[Position]], findBox: Box, goalPos: Position): List[Position] = {
    node.boxes.remove(node.boxes.indexOf(findBox))
    node.boxes.foreach(box => box.setMovable(false))

    val (_, edges) = Graph.construct(node)
    val boxPath = Astar.search(edges, findBox.getPosition, goalPos) match {
      case Nil =>
        val path = Astar.search(ignoreBoxEdges, findBox.getPosition, goalPos)
        val issues = node.boxes.filter(box => path.contains(box.getPosition))
        val newPath = findPathWithLimits(path, issues.toList, node, findBox, goalPos)
        issues.foreach(box => Node.walls.remove(box.getPosition))
        newPath
      case list => list
    }

    node.boxes.foreach(box => box.setMovable(true))
    node.boxes.add(findBox)
    boxPath
  }

  def findPath2(node: Node, findBox: Box, goalPos: Position, edges: Map[Position, List[Position]]): List[Position] = {
    val hashSet = new mutable.HashSet[Position]()
    node.boxes.map(box => hashSet += box.getPosition)
    Astar.search2(edges, findBox.getPosition, goalPos, hashSet)
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: List[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]], threshold: Int) = {
    Node.walls.addAll(dangerZone)
    val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }
    savedGoals.foreach { case (goalPos, _) => Node.goals.remove(goalPos) }
    if ( node.boxes.length != 1 ) {
      val boxId = goalMatch.get(goal).get
      val box = node.boxes.filter(box => box.getId == boxId).last
      val boxPath = findPath2(node, box, goal, edges)
      val agentPath = findPath2(node, box, node.getAgent.getPosition, edges)
      node.boxes.filter(box => !boxPath.contains(box.getPosition)
                               && !agentPath.contains(box.getPosition)
                               && box.getId != boxId)
                .foreach(box => box.setMovable(false))
      val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
      node.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
    }

    val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(goalMatch, edges))
    val solution = Search.search(strategy, node, threshold)
    savedGoals.foreach { case (goalPos,goalChar) => Node.goals.put(goalPos, goalChar) }
    node.boxes.foreach(box => box.setMovable(true))
    Node.walls.removeAll(dangerZone)
    solution
  }

  def findGoal(dependencies: Map[Position, List[Position]], solutionLength: Map[Position, Int]): List[Position] = {
    dependencies.filter { case (_, dependency) => dependency.isEmpty }.keys.toList.sortBy(pos => solutionLength(pos))
  }

  def matchGoalsWithBoxes(initialState: Node, edges: Map[Position, List[Position]]): Map[Position, Int] = {
    var goalMatch = Map[Position, Int]() // Goal position, Box
    Node.goals.foreach { case (goalPos, goalChar) =>
      def getBestBox(currentBest: Box, boxes: List[Box]): Int = {
        boxes match {
          case Nil => currentBest.getId
          case car :: cdr if currentBest == null =>
            val currentPath = Astar.search(edges, goalPos, car.getPosition)
            car.setGoalPath(currentPath)
            getBestBox(car, cdr)
          case car :: cdr =>
            val currentPath = Astar.search(edges, goalPos, car.getPosition)
            car.setGoalPath(currentPath)
            currentPath.length < currentBest.getGoalPath.length match {
              case true => getBestBox(car, cdr)
              case false => getBestBox(currentBest, cdr)
            }
        }
      }
      val boxes = initialState.boxes.filter(box => goalChar.equals(Character.toLowerCase(box.getCharacter))
                                                   && !goalMatch.values.contains(box.getId)).toList
      goalMatch += (goalPos -> getBestBox(null, boxes))
    }
    goalMatch
  }

  def getGoalDependencies(permutations: List[Position], goalMatch: Map[Position, Int],
                          initialState: Node): (Map[Position, List[Position]], Map[Position, Int]) = {
    var goalMatches: Map[Position, Int] = goalMatch
    def calcDependency(goalDependencies: Map[Position, List[Position]],
                       goals: List[Position], restarted: Boolean): (Map[Position, List[Position]], Map[Position, Int]) = {
      goals match {
        case Nil => (goalDependencies, goalMatches)
        case car :: cdr =>
          val (dependencies, newGoalMatches, hasChanged) = getGoalDependency(car, permutations, goalMatches, initialState)
          if ( hasChanged ) {
            if ( restarted ) {
              sys.error("Possible circular dependency on character of the same letter")
            }
            goalMatches = newGoalMatches
            calcDependency(Map(), permutations, restarted = true)
          }
          calcDependency(goalDependencies + (car -> dependencies), cdr, restarted = false)
      }
    }
    if ( permutations.size == 1 ) {
      (Map(permutations.head -> List()), goalMatches)
    } else {
      calcDependency(Map(), permutations, false)
    }
  }

  def getGoalDependency(permutation: Position, goals: List[Position], goalMatch: Map[Position, Int],
                        initialState: Node): (List[Position], Map[Position, Int], Boolean) = {
    Node.walls.add(permutation)
    val emptyStartState = new Node(initialState.parent)
    emptyStartState.setAgent(initialState.getAgent)
    val (_, edges) = Graph.construct(emptyStartState)

    def findDependencies(dependencies: List[Position], goals: List[Position]): List[Position] = {
      goals match {
        case Nil => dependencies
        case car :: cdr =>
          Astar.search(edges, car,
            initialState.boxes.find(box => box.getId == goalMatch(car)).get.getPosition) match {
            case null | Nil => findDependencies(car :: dependencies, cdr)
            case _ => findDependencies(dependencies, cdr)
          }
      }
    }
    val dependencies = findDependencies(List(), goals.filter(goal => !goal.equals(permutation)))
    Node.walls.remove(permutation)
    val goalChar = Node.goals.get(permutation)
    if ( dependencies.length == 1 ) {
      val matchingGoal = dependencies.find(goal => Node.goals.get(goal).equals(goalChar))
      matchingGoal match {
        case None => (dependencies, goalMatch, false)
        case Some(goal) =>
          val (goalDependencies, _, _)  = getGoalDependency(goal, goals, goalMatch, initialState)
          if ( goalDependencies.isEmpty ) {
            (dependencies, goalMatch, false)
          } else {
            val boxIdx = goalMatch.get(permutation).get
            val newBoxIdx = goalMatch.get(goal).get
            var newGoalMatch = goalMatch.filter { case (goalPos, id) => (!permutation.equals(goalPos) && id != boxIdx) ||
                                                                        (!goal.equals(goalPos) && id != newBoxIdx) }
            newGoalMatch = newGoalMatch + (permutation -> newBoxIdx) + (goal -> boxIdx)
            (dependencies, newGoalMatch, true)
          }
      }
    } else {
      (dependencies, goalMatch, false)
    }
  }
}

class LearnClient(searchClient: SearchClient) {
  val startState = searchClient.initialState
  val emptyStartState = new Node(startState.parent)
  emptyStartState.setAgent(startState.getAgent)
}