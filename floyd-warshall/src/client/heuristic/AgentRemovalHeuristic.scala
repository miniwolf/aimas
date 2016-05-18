package client.heuristic

import client.PathFinding
import searchclient.{Box, Node, Position}

import scala.collection.JavaConversions._
import scala.collection.immutable.HashSet


/**
  * Created by miniwolf on 03-05-2016.
  */
class AgentRemovalHeuristic(boxToRemove: Box, boxPath: List[Position], dangerZone: HashSet[Position],
                     issueBoxes: List[Box], edges: Map[Position, List[Position]]) extends Heuristic {
  override def h(n: Node): Int = {
    val matchingBox = n.boxes.find(box => box.getId == boxToRemove.getId).get
    val numberOfIssues = issueBoxes.count(box => dangerZone.contains(box.getPosition))
    20 * PathFinding.findPath2(n, matchingBox, n.getAgent.getPosition, edges).length + numberOfIssues
  }
}

