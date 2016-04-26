import _root_.Strategy.AdvancedStrategy
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
    val solutionLength: Map[Position, Int] = findSolutionLengths(goalMatch, node)
    val (dependencies, goalMatches) = Dependency.getGoalDependencies(goals, goalMatch, node)
    val goalsToSolve = findGoal(dependencies, solutionLength)

    val emptyState = new Node(node.parent)
    emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
    val (_, edges) = Graph.construct(emptyState)
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
          val path = Astar.search(edges, goalPos, box.getPosition)
          box.setGoalPath(path)
          path
        case path => path.toList
      }
      val agentPath = Astar.search(edges, state.getAgent.getPosition, box.getPosition)

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
                solveBestGoal(cdr, (goalPos, newSolution.toList), bestSoFar._2.length, node, goalMatches, solvedGoals, solutionLength, goalsWithBoxes)
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
    val pathToBox = Astar.search(edges, goalPos, box.getPosition)
    val pathToAgent = Astar.search(edges, agentPos, box.getPosition)
    val diff = vertices.diff(newVertices).diff(pathToBox).diff(pathToAgent)
    Node.walls.remove(goalPos)
    diff.filter(p => !p.equals(goalPos))
  }

  def removeBoxesFromPath(list: List[Box], solutionList: List[Node], goal: Position, node: Node, agentPath: HashSet[Position]) = {
    list match {
      case Nil => solutionList
      case box :: cdr =>
        val lockedNode = node.ChildNode()
        lockedNode.boxes.foreach { case box2 =>
          box2.setMovable(false)
                                 }
        lockedNode.boxes.remove(box)

        val (_, edges2) = Graph.construct(lockedNode)
        val x = box.getPosition.getX
        val y = box.getPosition.getY
        val testPositions = List[Position](new Position(x+1, y), new Position(x-1, y), new Position(x, y+1), new Position(x, y-1))
        //          var added = false
        val tempBoxGoal = testPositions.map(position => Astar.search3(edges2, position, agentPath)).filter(position => position != null).head
        val removedChar = Node.goals.remove(goal)
        Node.goals.put(tempBoxGoal, Character.toLowerCase(box.getCharacter))
        //          box.setMovable(true)
        lockedNode.boxes.add(box)
        val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(Map(tempBoxGoal -> box.getId), edges2))
        val solution = Search.search(strategy, lockedNode, 200000)
    }
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: List[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]], threshold: Int) = {
    Node.walls.addAll(dangerZone)
    val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }
    savedGoals.foreach { case (goalPos, _) => Node.goals.remove(goalPos) }
    if ( node.boxes.length != 1 ) {
      val boxId = goalMatch.get(goal).get
      val box = node.boxes.filter(box => box.getId == boxId).last
      val boxPath = PathFinding.findPath2(node, box, goal, edges)
      val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
      node.boxes.filter(box => !boxPath.contains(box.getPosition)
                            && !agentPath.contains(box.getPosition)
                            && box.getId != boxId)
                .foreach(box => box.setMovable(false))
      val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
      node.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
      val boxesOnAgentPath = node.boxes.filter(box => agentPath.contains(box.getPosition) && box.getId != boxId).toList
      val boxesOnBoxPath = node.boxes.filter(box => boxPath.contains(box.getPosition) && box.getId != boxId).toList
      if ( boxesOnAgentPath.nonEmpty ) {
        val list: List[Box] = agentPath.map(pos => boxesOnAgentPath.filter(box => box.getPosition.equals(pos)))
                                       .filter(list => list.nonEmpty).map(list => list.head)
        removeBoxesFromPath(list, List(), goal, node, HashSet() ++ agentPath)
      }
      if ( boxesOnBoxPath.size > 1 ) {
        val list = boxPath.map(pos => boxesOnBoxPath.filter(box => box.getPosition.equals(pos))).filter(list => list.nonEmpty).map(list => list.head)
        remo
      }
    }

    val strategy = new AdvancedStrategy(new AdvancedHeuristic.AStar(goalMatch, edges))
    val solution = Search.search(strategy, node, threshold)
    savedGoals.foreach { case (goalPos,goalChar) => Node.goals.put(goalPos, goalChar) }
    node.boxes.foreach(box => box.setMovable(true))
    Node.walls.removeAll(dangerZone)
    solution
  }
}
