package client

import searchclient.{Box, Node, Position}

import scala.collection.immutable.HashSet

/**
  * Created by miniwolf on 06-05-2016.
  */

object RemovalState {
  def apply(list: List[Box], solved: List[Box], node: Node, boxPath: HashSet[Position],
            goalBoxId: Int, immovableBoxes: List[(Int, Position)], parent: RemovalState) =
    new RemovalState(list, solved, node, boxPath, goalBoxId, immovableBoxes, parent)
}

class RemovalState(val list: List[Box], val solved: List[Box], val node: Node,
                   val boxPath: HashSet[Position], val goalBoxId: Int,
                   val immovableBoxes: List[(Int, Position)], val parent: RemovalState)
