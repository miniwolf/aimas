import Strategy.AdvancedStrategy
import searchclient.{Agent, Box, Node, Position}

import scala.collection.immutable.{HashSet, Map}
import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 26-04-2016.
  */
object Solution {
  def findSolution(goals: List[Position], goalMatch: Map[Position, Int], solution: List[Node],
                   solvedGoals: List[Position], node: Node): List[Node] = {
    if ( goals.isEmpty ) {
      return solution
    }
    val solutionLength = findSolutionLengths(goalMatch, node)
    val (dependencies, goalMatches) = Dependency.getGoalDependencies(goals, goalMatch, node)
    val goalsToSolve = findGoal(dependencies, solutionLength)

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

  def findGoal(dependencies: Map[Position, List[Position]], solutionLength: Map[Position, Int]): List[Position] = {
    dependencies.filter(dependency => dependency._2.isEmpty).keys.toList.sortBy(pos => solutionLength(pos))
  }

  def reduceGoalsToSolve(goalsToSolve: List[Position], goalsToIgnore: List[Position], node: Node, goalMatches: Map[Position, Int]): List[Position] = {
    goalsToSolve match {
      case Nil => goalsToIgnore
      case goalPos :: cdr =>
        val emptyState = new Node(node.parent)
        emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
        val (_, edges) = Graph.construct(emptyState)
        val boxId = goalMatches.get(goalPos).get
        val box = node.boxes.filter(box => box.getId == boxId).last
        val boxPath = PathFinding.findPath2(node, box, goalPos, edges)
        val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
        node.boxes.filter(box => boxPath.contains(box.getPosition) ||
                                 agentPath.contains(box.getPosition)).toList match {
          case Nil => reduceGoalsToSolve(cdr, goalsToIgnore, node, goalMatches)
          case _ => reduceGoalsToSolve(cdr, goalPos :: goalsToIgnore, node, goalMatches)
        }
    }
  }

  def findSolutionLengths(goalMatches: Map[Position, Int], state: Node): Map[Position, Int] = {
    val emptyStartState = new Node(state.parent)
    emptyStartState.setAgent(state.getAgent)
    val (_, edges) = Graph.construct(emptyStartState)
    goalMatches.map { case (goalPos, _) =>
      val box = state.boxes.find(box => box.getId == goalMatches(goalPos)).get
      val goalPath: List[Position] = box.getGoalPath match {
        case null =>
          val path = PathFinding.findPath2(state, box, goalPos, edges)
          box.setGoalPath(path)
          path
        case path => path.toList
      }
      val agentPath = PathFinding.findPath2(state, box, state.getAgent.getPosition, edges)

      goalPos -> (goalPath.length + agentPath.length)
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
                solveBestGoal(cdr, (goalPos, newSolution), newSolution.length, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
              } else {
                solveBestGoal(cdr, bestSoFar, threshold, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
              }
          }
        }
    }
  }

  def findDangerousPositions(vertices: List[Position], edges: Map[Position, List[Position]],
                             goalPos: Position, box: Box, safeSpot: Position, empty: Node) = {
    Node.walls.add(goalPos)
    val agentPos = empty.getAgent.getPosition
    empty.getAgent.setPosition(safeSpot)
    val (newVertices, _) = Graph.construct(empty)
    val pathToBox = PathFinding.findPath2(empty, box, goalPos, edges)
    val pathToAgent = PathFinding.findPath2(empty, box, agentPos, edges)
    val diff = vertices.diff(newVertices).diff(pathToBox).diff(pathToAgent)
    Node.walls.remove(goalPos)
    diff.filter(p => !p.equals(goalPos))
  }

  def removeBoxesFromPath(list: List[Box], solutionList: List[Node], goal: Position, node: Node, path: HashSet[Position], depth: Int): (List[Node], Node) = {
    list match {
      case Nil => (solutionList, node)
      case box :: cdr =>
        val lockedNode = node.ChildNode()
        lockedNode.parent = null
        lockedNode.boxes.foreach(b => b.setMovable(false))
        lockedNode.boxes.remove(box)

        val (_, edges) = Graph.construct(lockedNode)
        val x = box.getPosition.getX
        val y = box.getPosition.getY
        val testPositions = List[Position](new Position(x+1, y), new Position(x-1, y), new Position(x, y+1), new Position(x, y-1))
        val tempBoxGoalList = testPositions.map(position => Astar.search3(edges, position, HashSet() ++ path, depth)).filter(position => position != null)

        if ( tempBoxGoalList.isEmpty ) {
          return (null, node)
        }
        val tempBoxGoal = tempBoxGoalList.head
        val removedChar = Node.goals.remove(goal)
        Node.goals.put(tempBoxGoal, Character.toLowerCase(box.getCharacter))
        lockedNode.boxes.add(box)

        val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(Map(tempBoxGoal -> box.getId), edges))
        val solution = Search.search2(strategy, lockedNode, 200000).toList

        Node.goals.put(goal, removedChar)
        Node.goals.remove(tempBoxGoal)

        solution.head.parent = solutionList match {
          case Nil => null
          case _ => solutionList.last
        }
        val newNode = solution.last.ChildNode()
        newNode.parent = null
        removeBoxesFromPath(cdr, solution, goal, newNode, path, depth)
    }
  }

  def reducePath(path: List[Position], node: Node, boxId: Int, goal: Position, depth: Int): (List[Node], Node) = {
    node.boxes.filter(box => path.contains(box.getPosition) && box.getId != boxId).toList match {
      case Nil => (List(), node)
      case boxes =>
        val list: List[Box] = path.map(pos => boxes.filter(box => box.getPosition.equals(pos)))
                                  .filter(list => list.nonEmpty).map(list => list.head)
        removeBoxesFromPath(list, List(), goal, node, HashSet() ++ path, depth)
    }
  }

  def reducesPaths(agentPath: List[Position], boxPath: List[Position], node: Node, boxId: Int,
                   goal: Position, threshold: Int, depth: Int, solution: List[Node]): (List[Node], Node) = {
    val currentNode = node
    solution match {
      case null =>
        reducePath(agentPath, currentNode, boxId, goal, depth) match {
          case (null, _) => reducesPaths(agentPath, boxPath, node, boxId, goal, threshold, depth + 1, solution)
          case (agentSolution, _) if agentSolution.length > threshold => (null, node)
          case (agentSolution, agentNode) =>
            reducePath(boxPath, agentNode, boxId, goal, 1) match {
              case (null, _) => reducesPaths(agentPath, boxPath, node, boxId, goal, threshold, depth + 1, solution)
              case (boxSolution, _) if boxSolution.length + agentSolution.length > threshold => (null, node)
              case (boxSolution, boxNode) =>
                val newSolution = agentSolution ++ boxSolution
                reducesPaths(agentPath, boxPath, boxNode, boxId, goal, threshold, depth + 1, newSolution)
            }
        }
      case solutionList => (solutionList, node)
    }
  }

  def resetNode(savedGoals: Map[Position, Character], node: Node, dangerZone: List[Position]): Unit = {
    Node.goals = Node.goals ++ savedGoals
    node.boxes.foreach(box => box.setMovable(true))
    Node.walls.removeAll(dangerZone)
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: List[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]], threshold: Int): List[Node] = {
    Node.walls.addAll(dangerZone)
    val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }.toMap
    savedGoals.foreach(goal => Node.goals.remove(goal._1))
    var solution = List[Node]()
    var currentNode = node
    if ( node.boxes.length != 1 ) {
      val boxId = goalMatch.get(goal).get
      val box = node.boxes.filter(box => box.getId == boxId).last
      val boxPath = PathFinding.findPath2(node, box, goal, edges).reverse
      val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
      node.boxes.filter(box => !boxPath.contains(box.getPosition)
                            && !agentPath.contains(box.getPosition)
                            && box.getId != boxId)
                .foreach(box => box.setMovable(false))
      val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
      node.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
      val (reducedSolution, reducedNode) = reducesPaths(agentPath, boxPath, node, boxId, goal, threshold, 1, null)
      if ( reducedSolution == null ) {
        resetNode(savedGoals, node, dangerZone)
        return null
      }
      currentNode = reducedNode
      solution = reducedSolution
    }

    val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(goalMatch, edges))
    val newSolution = Search.search(strategy, currentNode, threshold)

    resetNode(savedGoals, node, dangerZone)
    if ( newSolution == null ) {
      return null
    }
    solution ++ newSolution
  }
}
