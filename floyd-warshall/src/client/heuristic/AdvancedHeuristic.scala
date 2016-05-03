package client.heuristic

import client.PathFinding
import searchclient.{Box, Node, Position}

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 31-03-2016.
  */
class AdvancedHeuristic(goalMatch: Map[Position, Int], edges: Map[Position, List[Position]]) extends Heuristic {
  def h(n: Node): Int = {
    var value = 0
    Node.goals.foreach { case (goalPos, _) =>
      val matchingBox = n.boxes.find(box => box.getId == goalMatch(goalPos)).get
      val boxRoute = boxPath(n, matchingBox, goalPos)
      val agentPath = PathFinding.findPath2(n, matchingBox, n.getAgent.getPosition, edges)
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
          val path = PathFinding.findPath2(n, matchingBox, goalPos, edges)
          matchingBox.setGoalPath(path)
          path
        case list => list.toList
      }
    } else {
      val path = n.parent.boxes.find(box => box.getId == matchingBox.getId).get.getGoalPath match {
        case list if list.contains(matchingBox.getPosition) =>
          list.splitAt(list.indexOf(matchingBox.getPosition))._1.toList
        case _ => PathFinding.findPath2(n, matchingBox, goalPos, edges)
      }
      matchingBox.setGoalPath(path)
      path
    }
  }
}
