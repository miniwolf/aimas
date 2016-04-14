import java.io.{BufferedReader, InputStreamReader}
import java.util.function.ToIntFunction

import Strategy.AdvancedStrategy
import searchclient.SearchClient.Memory
import searchclient._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by miniwolf on 29-03-2016.
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
                                            List(), learnClient.startState)
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
                   solvedGoals: List[Position], unableToSolve: List[Position], node: Node): List[Node] = {
    if ( goals.isEmpty ) {
      return solution
    }
    val solutionLength: Map[Position, Int] = findSolutionLengths(goalMatch, node)
    val (dependencies, goalMatches) = getGoalDependencies(goals, goalMatch, node)
    findGoal(dependencies, solutionLength) match {
      case goalPos =>
        val emptyState = new Node(node.parent)
        emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
        val (vertices, edges) = Graph.construct(emptyState)
        val newGoals = goals.filter(goal => !goal.equals(goalPos))
        val dangerZone = if ( goalMatches.size != 1 ) {
          val safeBox = node.boxes.filter(box => goalMatches(goalPos) != box.getId && goalMatches.values.contains(box.getId)).last
          val box = node.boxes.find(b => goalMatches(goalPos) == b.getId).get
          findDangerousPositions(vertices, edges, goalPos, box, safeBox.getPosition, emptyState)
        } else {
          List()
        }
        solveReduced(goalPos, goalMatches, dangerZone, solvedGoals, node, edges) match {
          case null => sys.error("Cannot find solution, issue issue!")
          case newSolution if canReachAllRemainingBoxes(goalPos, solvedGoals, newSolution.getLast) =>
            newSolution.getFirst.parent = solution match {
              case Nil => null
              case _ => solution.last
            }

            val newNode = newSolution.getLast.ChildNode()
            newNode.parent = null
            Node.walls.add(goalPos)
            Node.goals.remove(goalPos)

            val newGoalMatches = goalMatches.filter { case (pos,_) => !pos.equals(goalPos) }
            val newSolvedGoals: List[Position] = goalPos :: solvedGoals
            findSolution(newGoals ++ unableToSolve, newGoalMatches, solution ++ newSolution.toList,
                         newSolvedGoals, List(), newNode)
          case _ => findSolution(newGoals, goalMatches, solution, solvedGoals,
                                 goalPos :: unableToSolve, node)
        }
    }
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: List[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]]) = {
    Node.walls.addAll(dangerZone)
    val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }
    savedGoals.foreach { case (goalPos, _) => Node.goals.remove(goalPos) }
    val agentPos = node.getAgent.getPosition
    val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
    if ( node.boxes.length != 1 ) {
      val boxId = goalMatch.get(goal).get
      val boxPos = node.boxes.filter(box => box.getId == boxId).last.getPosition
      val boxPath = Astar.search(edges, goal, boxPos)
      val agentPath = Astar.search(edges, agentPos, boxPos)
      node.boxes.filter(box => !boxPath.contains(box.getPosition)
                               && !agentPath.contains(box.getPosition)
                               && box.getId != boxId)
                .foreach(box => box.setMovable(false))
      node.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
    }

    val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(goalMatch, edges))
    val solution = Search.search(strategy, node)
    savedGoals.foreach { case (goalPos,goalChar) => Node.goals.put(goalPos, goalChar) }
    node.boxes.foreach(box => box.setMovable(true))
    Node.walls.removeAll(dangerZone)
    solution
  }

  def findGoal(dependencies: Map[Position, List[Position]], solutionLength: Map[Position, Int]): Position = {
    val emptyDependencies = dependencies.filter { case (_, dependency) => dependency.isEmpty }
    emptyDependencies.minBy { case (pos, _) => solutionLength.get(pos) }._1
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
            var newGoalMatch = goalMatch.filter { case (goalPos, id) => !permutation.equals(goalPos) && id != boxIdx }
            val newBoxIdx = goalMatch.get(goal).get
            newGoalMatch = newGoalMatch.filter { case (goalPos, id) => !goal.equals(goalPos) && id != newBoxIdx }
            newGoalMatch += (permutation -> newBoxIdx)
            newGoalMatch += (goal -> boxIdx)
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