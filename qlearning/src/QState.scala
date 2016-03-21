import java.util.function.Consumer

import searchclient.{Command, Node}

/**
  * @author miniwolf
  */
class QState(val actions: List[QAction] = List.empty, val instance: Node, val reward: Float) {

  @inline
  final def isGoal: Boolean = instance.isGoalState || actions.isEmpty

  override def toString: String =
    s"state: ${actions.mkString(" ")}\nInstance:\n ${instance.toString}"

  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[QState]
  }

  override def equals(other: Any): Boolean = other match {
    case that: QState =>
      if ( !(that canEqual this) ) return false
      val otherNode: Node = that.instance
      instance.agentCol == otherNode.agentCol && instance.agentRow == otherNode.agentRow &&
      instance.boxes.equals(otherNode.boxes)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime: Int = 31
    var result: Int = 1
    result = prime * result + instance.agentCol
    result = prime * result + instance.agentRow
    result = prime * result + instance.boxes.hashCode
    result
  }
}

case class QIndexedState(state: QState, iter: Int)

object QState {
  def apply(instance: Node, reward: Float): QState = {
    var actions = List[QAction]()
    instance.getExpandedActions.forEach(new Consumer[Command] {
      override def accept(t: Command): Unit = actions.+:=(QAction(t))
    })
    new QState(actions, instance, reward)
  }
}
