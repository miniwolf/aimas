import java.io.{BufferedReader, InputStreamReader}
import java.util.function.ToIntFunction

import Strategy.AdvancedStrategy
import searchclient._

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 29-03-2016.
  */
object LearnClient extends App {
  def canReachAllRemainingBoxes(goalPoses: List[Position], node: Node): Boolean = {
    goalPoses.foreach { case goalPos =>
      Node.walls.add(goalPos)
      node.boxes.filter(box => box.getPosition.equals(goalPos)).last.setMovable(false)
    }

    val emptyState = new Node(node.parent)
    emptyState.setAgent(node.getAgent)
    val (_, edges) = Graph.construct(emptyState)
    val res = node.boxes.filter(box => box.isMovable) match {
      case list if list.isEmpty => true
      case list => !list.exists(box => Astar.search(edges, node.getAgent.getPosition, box.getPosition).isEmpty)
    }

    goalPoses.foreach { case goalPos =>
      Node.walls.remove(goalPos)
      node.boxes.filter(box => box.getPosition.equals(goalPos)).last.setMovable(true)
    }
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

    val learnClient = new LearnClient(client)
    val (_, edges) = Graph.construct(learnClient.emptyStartState)
    val goalMatches: Map[Position, Int] = matchGoalsWithBoxes(learnClient.startState, edges)
    val goalList: List[Position] = Node.goals.map { case (goalPos, _) => goalPos }.toList
    val solutionLength: Map[Position, Int] = findSolutionLengths(goalMatches, learnClient.startState)
    val solvedGoals = List[Position]()

    val solution: List[Node] = findSolution(goalList, goalMatches, List(), solutionLength, List(),
                                            List(), learnClient.startState)

    solvedGoals.foreach(goalPos => Node.walls.remove(goalPos))
    QClient.useInitialPath(client, serverMessages, solution)
  }

  def findSolution(goals: List[Position], goalMatch: Map[Position, Int], solution: List[Node],
                   solutionLength: Map[Position, Int], solvedGoals: List[Position],
                   unableToSolve: List[Position], node: Node): List[Node] = {
    val dependencies = getGoalDependencies(goals, goalMatch, node)
    findGoal(dependencies, solutionLength) match {
      case null => solution
      case goalPos =>
        val emptyState = new Node(node.parent)
        emptyState.setAgent(node.getAgent)
        val (_, edges) = Graph.construct(emptyState)
        val newGoals = goals.filter(goal => !goal.equals(goalPos))
        solveReduced(goals, goalPos, goalMatch, node, edges) match {
          case null =>
            sys.error("Cannot find solution, issue issue!")
          case newSolution if canReachAllRemainingBoxes(goalPos :: solvedGoals, newSolution.getLast) =>
            newSolution.getFirst.parent = solution match {
              case Nil => null
              case _ => solution.last
            }

            val newNode = newSolution.getLast.ChildNode()
            newNode.parent = null
            Node.walls.add(goalPos)
            Node.goals.remove(goalPos)
            newNode.boxes.filter(box => box.getPosition.equals(goalPos))
              .foreach(box => box.setMovable(false))

            val newGoalMatches = goalMatch.filter { case (pos,_) => !pos.equals(goalPos) }
            val newSolutionLengths = solutionLength.filter(length => !length._1.equals(goalPos))
            val newSolvedGoals: List[Position] = goalPos :: solvedGoals
            findSolution(newGoals ++ unableToSolve, newGoalMatches, solution ++ newSolution.toList,
                         newSolutionLengths, newSolvedGoals, List(), newNode)
          case _ => findSolution(newGoals, goalMatch, solution, solutionLength, solvedGoals,
                                 goalPos :: unableToSolve, node)
        }
    }
  }

  def solveReduced(goalList: List[Position], goal: Position, goalMatch: Map[Position, Int],
                   node: Node, edges: Map[Position, List[Position]]) = {
    val goals = goalList.filter(g => !g.equals(goal))
    val savedGoals = goals.map(goalPos => goalPos -> Node.goals.remove(goalPos))
    val agentPos = node.getAgent.getPosition
    if ( node.boxes.length != 1 ) {
      val boxId = goalMatch.get(goal).get
      val boxPos = node.boxes.filter(box => box.getId == boxId).last.getPosition
      val boxPath = Astar.search(edges, goal, boxPos)
      val agentPath = Astar.search(edges, agentPos, boxPos)
      node.boxes.filter(box => !boxPath.contains(box.getPosition)
                               && !agentPath.contains(box.getPosition)
                               && box.getId != boxId)
                .foreach(box => box.setMovable(false))
    }

    val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(goalMatch, edges))
    val solution = Search.search(strategy, node)
    savedGoals.foreach{ case (goalPos,goalChar) => Node.goals.put(goalPos, goalChar) }
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
      val boxes = initialState.boxes.filter(box => goalChar.equals(Character.toLowerCase(box.getCharacter))).toList
      goalMatch += (goalPos -> getBestBox(null, boxes))
    }
    goalMatch
  }

  def getGoalDependencies(permutations: List[Position], goalMatch: Map[Position, Int],
                          initialState: Node): Map[Position, List[Position]] = {
    def calcDependency(goalDependencies: Map[Position, List[Position]],
                       permutations: List[Position]): Map[Position, List[Position]] = {
      permutations match {
        case Nil => goalDependencies
        case car :: cdr =>
          val dependencies: List[Position] = getGoalDependency(car, goalMatch, initialState)
          calcDependency(goalDependencies + (car -> dependencies), cdr)
      }
    }
    if ( permutations.size == 1 ) {
      Map(permutations.head -> List())
    } else {
      calcDependency(Map(), permutations)
    }
  }

  def getGoalDependency(permutation: Position, goalMatch: Map[Position, Int], initialState: Node): List[Position] = {
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
            case null => findDependencies(car :: dependencies, cdr)
            case _ => findDependencies(dependencies, cdr)
          }
      }
    }
    val goals = Node.goals.filter(goal => !goal._1.equals(permutation)).keys.toList
    val dependencies = findDependencies(List(), goals)

    Node.walls.remove(permutation)
    dependencies
  }
}

class LearnClient(searchClient: SearchClient) {
  val startState = searchClient.initialState
  val emptyStartState = new Node(startState.parent)
  emptyStartState.setAgent(startState.getAgent)
}