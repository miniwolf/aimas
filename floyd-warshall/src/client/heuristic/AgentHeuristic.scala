package client.heuristic

import java.util
import scala.collection.JavaConversions._

import client.PathFinding
import searchclient.{Box, Node, Position}


/**
  * Created by miniwolf on 03-05-2016.
  */
class AgentHeuristic(boxToRemove: Box, edges: Map[Position, List[Position]]) extends Heuristic {
  def h(n: Node): Int = {
    val agent = n.getAgent
    val agentPath: List[Position] = PathFinding.findPath2(n, boxToRemove, n.getAgent.getPosition, edges)
    val agentPathSet = new util.HashSet[Position]()
    agentPathSet.addAll(agentPath)
    agent.setAgentPath(agentPathSet)
    agentPath.length
  }
}
