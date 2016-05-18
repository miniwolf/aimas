package client.heuristic

import searchclient.{Node, Position}

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 31-03-2016.
  */
class AdvancedHeuristic2(goalState: Position, goalBoxId: Int) extends Heuristic {

  def getManhattanDistance(boxPos: Position, pos: Position) = Math.abs(boxPos.getX - pos.getX) + Math.abs(boxPos.getY - pos.getY)

  def h(n: Node): Int = {
    val pos = n.getAgent.getPosition
    val boxPos = n.boxes.filter(box => box.getId == goalBoxId).get(0).getPosition
    var fakeBoxes = n.boxes.filter(box => box.getId != goalBoxId)
    var result = 15 * (getManhattanDistance(boxPos, goalState) + getManhattanDistance(pos, boxPos))
    fakeBoxes.foreach(box => result = result + 35 - getManhattanDistance(box.getPosition, goalState) )
    result
  }
}

