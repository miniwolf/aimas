package client.heuristic

import searchclient.{Node, Position}

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 31-03-2016.
  */
class AdvancedHeuristic2(goalState: Position, goalBoxId: Int) extends Heuristic {

  def getManhattanDistance(boxPos: Position, pos: Position) = Math.abs(boxPos.getX - pos.getX) + Math.abs(boxPos.getY - pos.getY)

  def h(n: Node): Int = {
    val boxPos = n.boxes.find(box => box.getId == goalBoxId).get.getPosition
    val fakeBoxes = n.boxes.filter(box => box.getId != goalBoxId)
    val result = 15 * (getManhattanDistance(boxPos, goalState) + getManhattanDistance(n.getAgent.getPosition, boxPos))
    result + fakeBoxes.map(box => 35 - getManhattanDistance(box.getPosition, goalState)).sum
  }
}

