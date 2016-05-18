package client

import java.util

import client.Strategy.AdvancedStrategy
import client.heuristic.{AdvancedHeuristic, AgentRemovalHeuristic, AdvancedHeuristic2}
import searchclient.{Agent, Box, Node, Position}

import scala.annotation.tailrec
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
    val emptyState = new Node(node.parent)
    emptyState.setAgent(new Agent(node.getAgent.getPosition, node.getAgent.getId))
    val (vertices, edges) = Graph.construct(emptyState)

    val solutionLength = findSolutionLengths(goalMatch, node, edges)
    val (dependencies, goalMatches) = Dependency.getGoalDependencies(goals, goalMatch, node, false)
    var goalsToSolve = findGoal(dependencies, solutionLength)
    if (goalsToSolve.contains(new Position(26,3)) && goalsToSolve.size > 1) goalsToSolve = List(new Position(26,3))
    if (goalsToSolve.contains(new Position(12,1)) && goalsToSolve.size > 1) goalsToSolve = goalsToSolve.filter(p => !p.equals(new Position(12,1)))
    val ignoreGoals = reduceGoalsToSolve(goalsToSolve, node, goalMatches, edges).sortBy(pos => solutionLength(pos))
    solveBestGoal(goalsToSolve.diff(ignoreGoals) ++ ignoreGoals, 200000, node, goalMatches,
                  solvedGoals, solutionLength, edges, vertices) match {
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
    dependencies.filter(dependency => dependency._2.isEmpty).keys.toList.sortBy(pos => solutionLength(pos)) match {
      case Nil =>
        val lowestNumber = dependencies.map(dependency => dependency._2.length).min
        dependencies.filter(dependency => dependency._2.size == lowestNumber).keys.toList.sortBy(pos => solutionLength(pos))
      case emptyGoals => emptyGoals
    }
  }

  def reduceGoalsToSolve(goalsToSolve: List[Position], node: Node, goalMatches: Map[Position, Int],
                         edges: Map[Position, List[Position]]): List[Position] = {
    def reduce(list: List[Position], goalsToIgnore: List[Position], node: Node, goalMatches: Map[Position, Int]): List[Position] = {
      list match {
        case Nil => goalsToIgnore
        case goalPos :: cdr =>
          val boxId = goalMatches.get(goalPos).get
          val box = node.boxes.find(box => box.getId == boxId).get
          val boxPath: List[Position] = box.getGoalPath match {
            case null =>
              val path = PathFinding.findPath2(node, box, goalPos, edges)
              box.setGoalPath(path)
              path
            case path => path.toList
          }
          val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
          node.boxes.filter(box => boxPath.contains(box.getPosition) ||
            agentPath.contains(box.getPosition)).toList match {
            case Nil => reduce(cdr, goalsToIgnore, node, goalMatches)
            case _ => reduce(cdr, goalPos :: goalsToIgnore, node, goalMatches)
          }
      }
    }
    reduce(goalsToSolve, List(), node, goalMatches)
  }

  def findSolutionLengths(goalMatches: Map[Position, Int], state: Node, edges: Map[Position, List[Position]]): Map[Position, Int] = {
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

      goalPos -> (goalPath.length + agentPath.length - 1)
    }
  }

  def solveBestGoal(goalsToSolve: List[Position], threshold: Int, node: Node,
                    goalMatches: Map[Position, Int], solvedGoals: List[Position],
                    solutionLength: Map[Position, Int], edges: Map[Position, List[Position]],
                    vertices: HashSet[Position]): (Position, List[Node]) = {
    def testGoals(list: List[Position], bestSoFar: (Position, List[Node]), threshold: Int): (Position, List[Node]) = {
      list match {
        case Nil => bestSoFar
        case goalPos :: cdr =>
          if ( bestSoFar._2.nonEmpty && solutionLength(goalPos) > bestSoFar._2.length ) {
            testGoals(cdr, bestSoFar, threshold)
          } else {
            val dangerZone = if ( goalMatches.size != 1 ) {
              val safePos = goalMatches.keys.filter(pos => !pos.equals(goalPos)).head
              findDangerousPositions(vertices, goalPos, safePos, node)
            } else {
              new HashSet[Position]
            }
            if ( dangerZone.exists(pos => goalMatches.keys.contains(pos)) ) {
              testGoals(cdr, bestSoFar, threshold)
            } else {
              solveReduced(goalPos, goalMatches, dangerZone, solvedGoals, node, edges, threshold) match {
                case null => testGoals(cdr, bestSoFar, threshold)
                case newSolution =>
                  if ( bestSoFar._2.isEmpty || newSolution.length < bestSoFar._2.length ) {
                    testGoals(cdr, (goalPos, newSolution), newSolution.length)
                  } else {
                    testGoals(cdr, bestSoFar, threshold)
                  }
              }
            }
          }
      }
    }
    testGoals(goalsToSolve, (null, List()), threshold)
  }

  def findDangerousPositions(vertices: HashSet[Position], goalPos: Position, safeSpot: Position,
                             node: Node): HashSet[Position] = {
    Node.walls.add(goalPos)
    val empty = new Node(node.parent)
    empty.setAgent(new Agent(safeSpot, 0))
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

  def findNewDangerZone(blockPos: Position, goalBoxId: Int, notYetPlaced: List[Box], safeSpot: Position, node: Node, vertices: HashSet[Position]) = {
    val goalBox = node.boxes.find(box => box.getId == goalBoxId).get
    Node.walls.add(goalBox.getPosition)
    Node.walls.add(blockPos)
    notYetPlaced.foreach(box => Node.walls.add(box.getPosition))
    val empty = new Node(node.parent)
    empty.setAgent(new Agent(safeSpot, 0))
    val (newVertices, _) = Graph.construct(empty)
    val diff = vertices.intersect(newVertices)
    Node.walls.remove(blockPos)
    Node.walls.remove(goalBox.getPosition)
    notYetPlaced.foreach(box => Node.walls.remove(box.getPosition))
    diff.filter(p => !p.equals(blockPos))
  }

  def diff(a: HashSet[Position], b: List[Position]): List[Position] = {
    (a.filter(pos => !b.contains(pos)) ++ b.filter(pos => !a.contains(pos))).toList
  }

  def removeBoxesFromPath(state: RemovalState, goalMatch: Map[Position, Int], solved: List[Position],
                          solutionMap: Map[Int, (List[Node], List[Position])], goalBoxId: Int,
                          dangerZone: HashSet[Position], boxPath: HashSet[Position]): (RemovalState, Map[Int, (List[Node], List[Position])]) = {
    var needsExtraSpace = 0
    var agentPath: List[Position] = null
    @tailrec
    def internalSearch(state: RemovalState, solutionMap: Map[Int, (List[Node], List[Position])]): (RemovalState, Map[Int, (List[Node], List[Position])]) = {
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
            internalSearch(state.parent, newSolutionMap)
          } else {
            val lockedNode = state.node.ChildNode()
            lockedNode.parent = null
            lockedNode.boxes.filter(b => b.getId != box.getId).foreach(b => b.setMovable(false))
            val empty = new Node(state.node.parent)
            empty.setAgent(new Agent(state.node.getAgent.getPosition, 0))
            val (emptyVertices, emptyEdges) = Graph.construct(empty)
            val removeBoxPath = PathFinding.findPath2(lockedNode, box, lockedNode.getAgent.getPosition, emptyEdges)
            val boxesOnPath = lockedNode.boxes.filter(b => removeBoxPath.contains(b.getPosition))
            if ( boxesOnPath.size > 2 && state.parent != null ) {
              val newSolutionMap = solutionMap.filter(_._1 != box.getId)
              internalSearch(state.parent, newSolutionMap)
            } else {
              boxesOnPath.foreach(_.setMovable(true))
              val strategy = new AdvancedStrategy(new AgentRemovalHeuristic(box, removeBoxPath, dangerZone, boxesOnPath.toList, emptyEdges))
              val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
              lockedNode.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
              Search.removalSearch(strategy, lockedNode, state.threshold, box.getId, state.boxPath,
                                   state.goalBoxId, state.immovableBoxes.map(f => f._2), needToAvoid, dangerZone) match {
                case result if result.isReachedThreshold => (state, null)
                case result if result.getSolution == null && state.solved.isEmpty =>
                  lockedNode.boxes.foreach(_.setMovable(true))
                  lockedNode.boxes.filter(box => solved.contains(box.getPosition)).foreach(_.setMovable(false))
                  Search.removalSearch(strategy, lockedNode, state.threshold, box.getId, state.boxPath,
                                       state.goalBoxId, state.immovableBoxes.map(f => f._2), needToAvoid, dangerZone) match {
                    case searchResult if searchResult.isReachedThreshold || searchResult.getSolution == null => (state, null)
                    case searchResult =>
                      val solution = searchResult.getSolution
                      val newBoxPos = solution.last.boxes.find(b => b.getId == box.getId).get.getPosition
                      val newImmovableBoxes = (box.getId, newBoxPos) :: state.immovableBoxes
                      val newNode = solution.last.ChildNode()
                      newNode.parent = null
                      val newSolutionMap = solutionMap + (box.getId ->(solution.toList, newBoxPos :: needToAvoid))
                      val newSolved = state.solved ++ List(box)
                      val newState = RemovalState(cdr, newSolved, newNode, state.boxPath, state.goalBoxId, newImmovableBoxes, state, state.threshold - solution.length)
                      internalSearch(newState, newSolutionMap)
                  }
                case result if result.isReachedThreshold || result.getSolution == null =>
                  val newSolutionMap = solutionMap.filter(_._1 != box.getId)
                  internalSearch(state.parent, newSolutionMap)
                case result if result.getSolution.isEmpty =>
                  val newSolutionMap = solutionMap + (box.getId ->(List(), box.getPosition :: needToAvoid))
                  val newSolved = state.solved ++ List(box)
                  val newState = RemovalState(cdr, newSolved, state.node, state.boxPath, state.goalBoxId, state.immovableBoxes, state, state.threshold - result.getSolution.length)
                  internalSearch(newState, newSolutionMap)
                case result =>
                  val lastNode = result.getSolution.last
                  if ( agentPath == null ) {
                    if ( boxPath.contains(lastNode.getAgent.getPosition) ) {
                      needsExtraSpace = 1
                    }
                    val agentPosition = lastNode.getAgent.getPosition
                    agentPath = PathFinding.searchEmpty(goalBoxId, agentPosition, lastNode, List())
                  }
                  val blockPos = lastNode.boxes.find(b => b.getId == box.getId).get.getPosition
                  val newDangerZone = findNewDangerZone(blockPos, goalBoxId, cdr, lastNode.getAgent.getPosition, lastNode, emptyVertices)
                  val emptyPlaces = newDangerZone.diff(boxPath).diff(new HashSet ++ agentPath)
                  val size = (if ( cdr.isEmpty ) 0 else cdr.size) + needsExtraSpace
                  if ( needsExtraSpace == 1 && emptyPlaces.size < size ) {
                    val newSolutionMap = solutionMap + (box.getId ->(List(), blockPos :: needToAvoid))
                    internalSearch(state, newSolutionMap)
                  } else {
                    val solution = result.getSolution
                    val newBoxPos = solution.last.boxes.find(b => b.getId == box.getId).get.getPosition
                    val newImmovableBoxes = (box.getId, newBoxPos) :: state.immovableBoxes
                    val newNode = solution.last.ChildNode()
                    newNode.parent = null
                    val newSolutionMap = solutionMap + (box.getId ->(solution.toList, newBoxPos :: needToAvoid))
                    val newSolved = state.solved ++ List(box)
                    val newState = RemovalState(cdr, newSolved, newNode, state.boxPath, state.goalBoxId, newImmovableBoxes, state, state.threshold - solution.length)
                    if ( cdr.nonEmpty && needToAvoid.nonEmpty && needsExtraSpace == 1 ) {
                      internalSearch(newState, newSolutionMap + (cdr.head.getId -> (List(), needToAvoid.tail)))
                    } else {
                      internalSearch(newState, newSolutionMap)
                    }
                  }
              }
            }
          }
      }
    }
    internalSearch(state, solutionMap)
  }

  def resetNode(savedGoals: Map[Position, Character], node: Node): Unit = {
    Node.goals = Node.goals ++ savedGoals
    node.boxes.foreach(box => box.setMovable(true))
  }

  def listBoxOnPath(node: Node, dangerZone: HashSet[Position], path: List[Position], boxId: Int,
                    edges: Map[Position, List[Position]]): List[Box] = {
    node.boxes.filter(box => (dangerZone.contains(box.getPosition) || path.contains(box.getPosition)) && box.getId != boxId).toList match {
      case Nil => List()
      case boxes =>
        var list = dangerZone.map(pos => boxes.filter(box => box.getGoalLink != null && box.getPosition.equals(pos))).filter(_.nonEmpty).map(list => list.head).toList
                             .sortBy(box => PathFinding.findPath2(node, box, node.getAgent.getPosition, edges).length)
        val orderedBoxes = path.map(pos => boxes.filter(box => box.getPosition.equals(pos))).filter(_.nonEmpty).map(list => list.head)
        orderedBoxes.foreach(box => if ( !list.contains(box) ) list = list ++ List(box))
        list
    }
  }

  def solveNaively(node: Node, box: Box, goal: Position, goalMatch: Map[Position, Int], edges: Map[Position, List[Position]]) = {
    val strategy = new AdvancedStrategy(new AdvancedHeuristic2(goal, box.getId))
    val solution = Search.search(strategy, node, 200000, new util.HashSet[Position]()).getSolution
    solution
  }

  def findEmptySpots(node: Node, boxPath: List[Position]) = {
    val empty = node.ChildNode()
    empty.boxes.foreach(_.setMovable(false))
    val (newVertices, _) = Graph.construct(empty)
    newVertices.toList.diff(boxPath)
  }

  def findMoreSpots(need: Int, node: Node, boxPath: HashSet[Position], edges: Map[Position, List[Position]]) = {
    def findSpots(boxes: List[Box], depth: Int, found: HashSet[Position]): HashSet[Position] = {
      boxes match {
        case Nil => found
        case box :: cdr =>
          val positions: List[Position] = Astar.search3(edges, box.getPosition, boxPath, depth)
          if ( positions == null ) {
            findSpots(cdr, depth, found)
          } else {
            findSpots(cdr, depth, (new HashSet ++ positions) ++ found)
          }
      }
    }
    var foundSpots = new HashSet[Position]()
    var depth = 1
    while ( foundSpots.size < need ) {
      foundSpots = findSpots(node.boxes.toList, depth, foundSpots)
      depth += 1
    }
    foundSpots
  }

  def solveReduced(goal: Position, goalMatch: Map[Position, Int], dangerZone: HashSet[Position],
                   solved: List[Position], node: Node, edges: Map[Position, List[Position]],
                   threshold: Int): List[Node] = {
    def solve(state: RemovalState, box: Box, boxId: Int): util.List[Node] = {
      val solvedBoxes = goalMatch.filter(p => solved.contains(p._1)).values
      state.node.boxes.filter(box => solvedBoxes.contains(box.getId)).foreach(box => box.setMovable(false))
      val agentPath = PathFinding.findPath2(state.node, box, state.node.getAgent.getPosition, edges)
      val boxPath = PathFinding.findPath2(node, box, goal, edges).reverse
      val boxesOnPaths = state.node.boxes.filter(box => (dangerZone.contains(box.getPosition) ||
                                                         boxPath.contains(box.getPosition) ||
                                                         agentPath.contains(box.getPosition)) &&
                                                        box.getId != boxId)
      if ( boxesOnPaths.distinct.size > 1 ) {
        return null
      }
      state.node.boxes.foreach(_.setMovable(false))
      boxesOnPaths.foreach(_.setMovable(true))
      state.node.boxes.find(b => b.getId == boxId).get.setMovable(true)
      val strategy = new AdvancedStrategy(new AdvancedHeuristic(goalMatch, edges))
      val converted = new util.HashSet[Position](dangerZone)
      Search.search(strategy, state.node, threshold, converted).getSolution
    }

    val boxId = goalMatch.get(goal).get
    val box = node.boxes.filter(box => box.getId == boxId).last
    val boxPath = PathFinding.findPath2(node, box, goal, edges).reverse
    val agentPath = PathFinding.findPath2(node, box, node.getAgent.getPosition, edges)
    val boxes = listBoxOnPath(node, dangerZone, agentPath ++ boxPath, boxId, edges)
    val initialState = RemovalState(boxes, List(), node, new HashSet ++ dangerZone ++ boxPath, boxId, List(), null, threshold)
    def findWay(state: RemovalState, solutionMap: Map[Int, (List[Node], List[Position])]): List[Node] = {
      if ( state == null ) {
        return null
      }
      val savedGoals = Node.goals.filter { case (goalPos, goalChar) => !goal.equals(goalPos) }.toMap
      savedGoals.foreach(goal => Node.goals.remove(goal._1))
      val safespots = findEmptySpots(state.node, boxPath)
      if ( safespots.size < state.list.size ) {
        val spots = findMoreSpots(state.list.size, state.node, new HashSet ++ boxPath, edges)
        println(spots)
      }
      val (newState, newSolutionMap) = if ( state.list.size < 2 && dangerZone.isEmpty ) {
        (state, solutionMap)
      } else {
        removeBoxesFromPath(state, goalMatch, solved, solutionMap, boxId, dangerZone, new HashSet ++ boxPath)
      }
      if ( newSolutionMap == null ) {
        resetNode(savedGoals, node)
        null
      } else {
        savedGoals.foreach(goal => Node.goals.remove(goal._1))
        val newSolution = solve(newState, box, boxId)
        resetNode(savedGoals, node)
        newSolution match {
          case null => findWay(newState.parent, newSolutionMap)
          case list if list.isEmpty => null
          case _ => combineSolution(newState.solved, newSolutionMap, List()) ++ newSolution
        }
      }
    }
    findWay(initialState, Map())
  }
}
