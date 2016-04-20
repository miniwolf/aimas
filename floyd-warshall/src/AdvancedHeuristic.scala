import java.util.Comparator

import searchclient.{Box, Node, Position}

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 31-03-2016.
  */
object AdvancedHeuristic {
  class AStar(goalMatch: Map[Position, Int], edges: Map[Position, List[Position]]) extends Heuristic(goalMatch, edges) {
    def f(n: Node) = n.g + h(n)

    override def toString = "A* evaluation"
  }
}

abstract class Heuristic(goalMatch: Map[Position, Int], edges: Map[Position, List[Position]]) extends Comparator[Node] {
  def compare(n1: Node, n2: Node): Int = f(n1) - f(n2)

  def h(n: Node): Int = {
    var value = 0
    Node.goals.foreach { case (goalPos, _) =>
      val matchingBox = n.boxes.find(box => box.getId == goalMatch(goalPos)).get
      val boxRoute = boxPath(n, matchingBox, goalPos)
      val agentPath = Astar.search(edges, n.getAgent.getPosition, matchingBox.getPosition)
      n.boxes.filter(box => box.getId != matchingBox.getId &&
                            (agentPath.contains(box.getPosition) || boxRoute.contains(box.getPosition)))
             .foreach(_ => value += 1)
      value += boxRoute.length
      value += agentPath.length
    }
    value
  }

  def boxPath(n: Node, matchingBox: Box, goalPos: Position): List[Position] = {
    if ( n.parent.boxes.exists(box => box.getPosition.equals(matchingBox.getPosition)
                                      && box.getId == matchingBox.getId) ) {
      matchingBox.getGoalPath match {
        case list if list.isEmpty && !matchingBox.getPosition.equals(goalPos) =>
          val path = Astar.search(edges, goalPos, matchingBox.getPosition)
          matchingBox.setGoalPath(path)
          path
        case list => list.toList
      }
    } else {
      val parentBox = n.parent.boxes.find(box => box.getId == matchingBox.getId).get
      val path = parentBox.getGoalPath match {
        case list if list.contains(matchingBox.getPosition) =>
          list.splitAt(list.indexOf(matchingBox.getPosition))._1.toList
        case _ =>
          Astar.search(edges, goalPos, matchingBox.getPosition)
      }
      matchingBox.setGoalPath(path)
      path
    }
  }

  def f(n: Node): Int
}
