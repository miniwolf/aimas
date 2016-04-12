import searchclient.Node

import scala.collection.JavaConversions._
/**
  * @author miniwolf
  */
class QState(val instance: Node, val reward: Float) {
  @inline
  final def isGoal: Boolean = instance.isGoalState

  override def toString: String =
    s"Reward: $reward - Instance:\n ${instance.toString}"

  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[QState]
  }

  override def equals(other: Any): Boolean = other match {
    case that: QState =>
      if ( !(that canEqual this) ) return false
      val otherNode: Node = that.instance
      instance.getAgent.equals(otherNode.getAgent) && instance.boxes.equals(otherNode.boxes)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime: Int = 31
    var result: Int = 1
    result = prime * result + instance.getAgent.hashCode
    result = prime * result + instance.boxes.hashCode
    result
  }
}

object QState {
  def getExpandedActions(node: Node): List[QAction] = {
    node.getExpandedNodes.map(n => QAction(n.action)).toList
  }
}
