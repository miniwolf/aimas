import searchclient.{Agent, Box, Node, Position}

import scala.collection.immutable.{HashSet, Map}
import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 26-04-2016.
  */
object PathFinding {
  def findPath(node: Node, ignoreBoxEdges: Map[Position, List[Position]], findBox: Box,
               goalPos: Position): List[Position] = {
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
    val hashSet = HashSet() ++ node.boxes.map(box => box.getPosition)
    Astar.search2(edges, findBox.getPosition, goalPos, hashSet)
  }

  def findPathWithLimits(path: List[Position], remainingBoxes: List[Box], node: Node, findBox: Box,
                         goal: Position): List[Position] = {
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
}
