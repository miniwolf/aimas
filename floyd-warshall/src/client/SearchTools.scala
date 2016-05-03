package client

import java.util.function.Predicate

import searchclient.{Box, Node, Position}

/**
  * Created by miniwolf on 03-05-2016.
  */
class SearchTools {
  def agentGoalState(leafNode: Node, goalBoxPos: Position, boxToRemoveId: Int)  = {
    val agentPath = leafNode.getAgent.getAgentPath
    val boxToRemove = leafNode.boxes.stream().filter(new Predicate[Box] {
      override def test(box: Box): Boolean = {
        box.getId == boxToRemoveId
      }
    }).findFirst().get()
  }
}
