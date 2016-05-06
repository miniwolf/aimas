package client

import client.Strategy.AdvancedStrategy
import client.heuristic.{AdvancedHeuristic, AgentHeuristic}
import searchclient.{Agent, Box, Node, Position}

import scala.collection.JavaConversions._
import scala.collection.immutable.{HashSet, Map}



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
        findSolution(newGoals, newGoalMatches, solution ++ newSolution, newSolvedGoals, newNode)
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
    empty.getAgent.setPosition(safeSpot)
    val (newVertices, _) = Graph.construct(empty)
    val diff = vertices.diff(newVertices)
    Node.walls.remove(goalPos)
    diff.filter(p => !p.equals(goalPos))
  }

  def combineSolution(list: List[Box], solutionMap: Map[Int, (List[Node], List[Position])], solution: List[Node]): List[Node] = {
    list match {
      case Nil => solution
      case box :: cdr =>
        combineSolution(cdr, solutionMap, solution ++ solutionMap(box.getId)._1)
    }
  }

  def removeBoxesFromPath(state: RemovalState, solutionMap: Map[Int, (List[Node], List[Position])]): (RemovalState, Map[Int, (List[Node], List[Position])]) = {
    state.list match {
      case Nil => (state, solutionMap)
      case box :: cdr =>
        val needToAvoid = solutionMap.get(box.getId) match {
          case None if state.solved.isEmpty => List()
          case None => List()
          case Some((_, listOfPos)) => listOfPos
        }
        if ( state.solved.nonEmpty && needToAvoid.size == solutionMap(state.solved.head.getId)._2.size ) {
          val newSolutionMap = solutionMap.filter(_._1 != box.getId)
          return removeBoxesFromPath(state.parent, newSolutionMap)
        }

        val lockedNode = state.node.ChildNode()
        lockedNode.parent = null
        lockedNode.boxes.filter(b => b.getId != box.getId).foreach(b => b.setMovable(false))
        val empty = new Node(state.node.parent)
        empty.setAgent(new Agent(state.node.getAgent.getPosition, 0))
        val (_, emptyEdges) = Graph.construct(empty)
        val realGoalBox = lockedNode.boxes.find(b => b.getId == state.goalBoxId).get
        val boxPath = PathFinding.findPath2(lockedNode, box, lockedNode.getAgent.getPosition, emptyEdges)
        lockedNode.boxes.filter(b => boxPath.contains(b.getPosition)).foreach(_.setMovable(true))
        val strategy = new AdvancedStrategy(new AgentHeuristic(box, boxPath, emptyEdges))
        Search.removalSearch(strategy, lockedNode, 200000, box.getId, state.boxPath,
                             realGoalBox.getPosition, state.immovableBoxes.map(f=>f._2), needToAvoid) match {
          case null if state.solved.isEmpty =>
            lockedNode.boxes.foreach(_.setMovable(true))
            Search.removalSearch(strategy, lockedNode, 200000, box.getId, state.boxPath,
                                 realGoalBox.getPosition, state.immovableBoxes.map(f=>f._2), needToAvoid) match {
              case null => (state, null)
              case solution =>
                val newBoxPos = solution.getLast.boxes.find(b => b.getId == box.getId).get.getPosition
                val newImmovableBoxes = (box.getId, newBoxPos) :: state.immovableBoxes
                val newNode = solution.last.ChildNode()
                newNode.parent = null
                val newSolutionMap = solutionMap + (box.getId -> (solution.toList, newBoxPos :: needToAvoid))
                val newSolved = state.solved ++ List(box)
                val newState = RemovalState(cdr, newSolved, newNode, state.boxPath, state.goalBoxId, newImmovableBoxes, state)
                removeBoxesFromPath(newState, newSolutionMap)
            }
          case null =>
            val newSolutionMap = solutionMap.filter(_._1 != box.getId)
            removeBoxesFromPath(state.parent, newSolutionMap)
          case solution if solution.isEmpty =>
            val newSolutionMap = solutionMap + (box.getId -> (List(), needToAvoid))
            val newSolved = state.solved ++ List(box)
            val newState = RemovalState(cdr, newSolved, state.node, state.boxPath, state.goalBoxId, state.immovableBoxes, state)
            removeBoxesFromPath(newState, newSolutionMap)
          case solution =>
            val newBoxPos = solution.getLast.boxes.find(b => b.getId == box.getId).get.getPosition
            val newImmovableBoxes = (box.getId, newBoxPos) :: state.immovableBoxes

            //        if ( solved.nonEmpty ) {
            //          solution.head.parent = solutionMap(solved.last.getId)._1.last
            //        }
            val newNode = solution.last.ChildNode()
            newNode.parent = null
            val newSolutionMap = solutionMap + (box.getId -> (solution.toList, newBoxPos :: needToAvoid))
            val newSolved = state.solved ++ List(box)
            val newState = RemovalState(cdr, newSolved, newNode, state.boxPath, state.goalBoxId, newImmovableBoxes, state)
            removeBoxesFromPath(newState, newSolutionMap)
        }
    }
  }

//  def removeBoxesFromPath(list: List[Box], solutionList: List[Node], goal: Position, node: Node,
//                          path: HashSet[Position], depth: Int, boxId: Int): (List[Node], Node, Int) = {
//    list match {
//      case Nil => (solutionList, node, depth)
//      case box :: cdr =>
//        val lockedNode = node.ChildNode()
//        lockedNode.parent = null
//        lockedNode.boxes.foreach(b => b.setMovable(false))
//        lockedNode.boxes.remove(box)
//
//        val empty = new Node(lockedNode.parent)
//        empty.setAgent(new Agent(lockedNode.getAgent.getPosition, 0))
//        val (_, emptyEdges) = Graph.construct(empty)
//        val pathToBox = PathFinding.findPath2(lockedNode, box, lockedNode.getAgent.getPosition, emptyEdges)
//        val boxesOnThePath = lockedNode.boxes.filter(b => pathToBox.contains(b.getPosition)).toList
//        lockedNode.boxes.removeAll(boxesOnThePath)
//
//        val (vertices, edges) = Graph.construct(lockedNode)
//        val x = box.getPosition.getX
//        val y = box.getPosition.getY
//        val testPositions = List[Position](new Position(x+1, y), new Position(x-1, y), new Position(x, y+1), new Position(x, y-1))
//        val tempBoxGoalList = testPositions.map(position => Astar.search3(edges, position, HashSet() ++ path, depth)).filter(position => position != null)
//
//        if ( tempBoxGoalList.isEmpty ) {
//          return (null, node, depth)
//        }
//        val tempBoxGoal = tempBoxGoalList.head
//        val removedChar = Node.goals.remove(goal)
//        Node.goals.put(tempBoxGoal, Character.toLowerCase(box.getCharacter))
//        lockedNode.boxes.add(box)
//        boxesOnThePath.foreach(b => b.setMovable(true))
//        lockedNode.boxes.addAll(boxesOnThePath)
//
//        val realGoalBox = lockedNode.boxes.find(b => b.getId == boxId).get
//        val dangerZones = findDangerousPositions(vertices, edges, tempBoxGoal, realGoalBox, box.getPosition, lockedNode.ChildNode())
//        var currentDepth = depth
//        if ( depth > 50 ) {
//          lockedNode.boxes.foreach(b => b.setMovable(true))
//          currentDepth = 1
//        }
//
//        val strategy = new AdvancedStrategy(new AdvancedHeuristic(Map(tempBoxGoal -> box.getId), edges))
//        val solution: util.LinkedList[Node] = Search.search(strategy, lockedNode, 200000, dangerZones)
//
//        Node.goals.put(goal, removedChar)
//        Node.goals.remove(tempBoxGoal)
//
//        if ( solution == null ) {
//          return (null, node, currentDepth)
//        }
//        if ( solution.isEmpty ) {
//          return removeBoxesFromPath(cdr, solutionList, goal, node, path, currentDepth, boxId)
//        }
//        solution.head.parent = solutionList match {
//          case Nil => null
//          case _ => solutionList.last
//        }
//        val newNode = solution.last.ChildNode()
//        newNode.parent = null
//        removeBoxesFromPath(cdr, solutionList ++ solution.toList, goal, newNode, path, currentDepth, boxId)
//    }
//  }

  def resetNode(savedGoals: Map[Position, Character], node: Node): Unit = {
    Node.goals = Node.goals ++ savedGoals
    node.boxes.foreach(box => box.setMovable(true))
  }

  def listBoxOnPath(node: Node, path: List[Position], boxId: Int): List[Box] = {
    node.boxes.filter(box => path.contains(box.getPosition) && box.getId != boxId).toList match {
      case Nil => List()
      case boxes =>
        var list = List[Box]()
        val orderedBoxes = path.map(pos => boxes.filter(box => box.getPosition.equals(pos))).filter(_.nonEmpty).map(list => list.head)
        orderedBoxes.foreach { case box =>
          if ( !list.contains(box) ) {
            list = list ++ List(box)
          }
        }
        list
    }
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: List[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]],
                   threshold: Int): List[Node] = {
    def solve(currentNode: Node, box: Box, boxId: Int) = {
      val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
      currentNode.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
      val agentPath = PathFinding.findPath2(currentNode, box, currentNode.getAgent.getPosition, edges)
      val boxPath = PathFinding.findPath2(node, box, goal, edges).reverse
      currentNode.boxes.filter(box => !boxPath.contains(box.getPosition) &&
                                      !agentPath.contains(box.getPosition) &&
                                      box.getId != boxId)
        .foreach(box => box.setMovable(false))
      val strategy = new AdvancedStrategy(new AdvancedHeuristic(goalMatch, edges))
      Search.search(strategy, currentNode, threshold, dangerZone)
    }

    val boxId = goalMatch.get(goal).get
    val box = node.boxes.filter(box => box.getId == boxId).last
    val boxPath = PathFinding.findPath2(node, box, goal, edges).reverse
    val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
    val boxes = listBoxOnPath(node, agentPath ++ boxPath, boxId)
    val initialState = RemovalState(boxes, List(), node, new HashSet ++ boxPath, boxId, List(), null)
    def findWay(state: RemovalState, solutionMap: Map[Int, (List[Node], List[Position])]): List[Node] = {
      val (newState, newSolutionMap) = if ( state.list.size == 1 ) {
        (state, solutionMap)
      } else {
        removeBoxesFromPath(state, solutionMap)
      }
      val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }.toMap
      savedGoals.foreach(goal => Node.goals.remove(goal._1))
      val newSolution = solve(newState.node, box, boxId)
      resetNode(savedGoals, node)
      newSolution match {
        case null => findWay(newState.parent, newSolutionMap)
        case list if list.isEmpty => null
        case _ => combineSolution(newState.solved, newSolutionMap, List()) ++ newSolution
      }
    }
    findWay(initialState, Map())
  }
}
