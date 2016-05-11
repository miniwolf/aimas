package client

import core._
import core.singleagent.{SingleAgent, SingleNode}

import scala.collection.immutable.{HashSet, Map}
import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 26-04-2016.
  */
object PathFinding {
  def findPath(node: SingleNode, ignoreBoxEdges: Map[Position, List[Position]], findBox: Box,
               goalPos: Position): List[Position] = {
    node.getBoxes.remove(node.getBoxes.indexOf(findBox))
    node.getBoxes.foreach(box => box.setMovable(false))

    val (_, edges) = Graph.construct(node)
    val boxPath = findPath2(node, findBox, goalPos, edges) match {
      case Nil =>
        val path = findPath2(node, findBox, goalPos, ignoreBoxEdges)
        val issues = node.getBoxes.filter(box => path.contains(box.getPosition))
        val newPath = findPathWithLimits(path, issues.toList, node, findBox, goalPos)
        issues.foreach(box => Node.walls.remove(box.getPosition))
        newPath
      case list => list
    }

    node.getBoxes.foreach(box => box.setMovable(true))
    node.getBoxes.add(findBox)
    boxPath
  }

  def findPath2(node: Node, findBox: Box, goalPos: Position, edges: Map[Position, List[Position]]): List[Position] = {
    val hashSet = HashSet() ++ node.getBoxes.map(box => box.getPosition)
    Astar.search(edges, findBox.getPosition, goalPos, hashSet)
  }

  def searchEmpty(goalBoxId: Int, goalPos: Position, node: Node, immovableBoxes: List[Position]) = {
    val emptyState = new SingleNode(node.parent)
    Node.walls.addAll(immovableBoxes)
    emptyState.setAgent(new SingleAgent(node.getAgent.getPosition, 0))
    val (_, edges) = Graph.construct(emptyState)
    Node.walls.removeAll(immovableBoxes)
    val goalBox = node.getBoxes.find(box => goalBoxId == box.getId).get
    val hashSet = HashSet() ++ node.getBoxes.map(box => box.getPosition)
    Astar.search(edges, goalBox.getPosition, goalPos, hashSet)
  }

  def findPathWithLimits(path: List[Position], remainingBoxes: List[Box], node: SingleNode, findBox: Box,
                         goal: Position): List[Position] = {
    remainingBoxes match {
      case Nil => path
      case box :: cdr =>
        Node.walls.add(box.getPosition)
        val emptyState = new SingleNode(node.parent)
        emptyState.setAgent(new SingleAgent(node.getAgent.getPosition, 0))
        val (_, edges) = Graph.construct(emptyState)
        findPath2(node, findBox, goal, edges) match {
          case Nil =>
            Node.walls.remove(box.getPosition)
            findPathWithLimits(path, cdr, node, findBox, goal)
          case newPath => findPathWithLimits(newPath, cdr, node, findBox, goal)
        }
    }
  }
}
