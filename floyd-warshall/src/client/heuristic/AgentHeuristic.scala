package client.heuristic

import scala.collection.JavaConversions._

import client.PathFinding
import searchclient.{Box, Node, Position}


/**
  * Created by miniwolf on 03-05-2016.
  */
class AgentHeuristic(boxToRemove: Box, edges: Map[Position, List[Position]]) extends Heuristic {
  def h(n: Node): Int = {
    val matchingBox = n.boxes.find(box => box.getId == boxToRemove.getId).get
    PathFinding.findPath2(n, matchingBox, n.getAgent.getPosition, edges).length
  }
}
