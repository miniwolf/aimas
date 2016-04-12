import java.util.function.Consumer

import searchclient.Command.dir
import searchclient.{Command, Node, Position}

import scala.collection.JavaConversions._
import scala.util.Random

/**
  * @author miniwolf
  */
class QState(val state: Node, qParent: QState) extends Node(if ( qParent == null ) null else qParent.state) {
  var children = List[QState]()
  var actions = List[QState]()
  var reward = 0.0f
  var numVisits = 0

  def expand: QState = {
    if ( actions.isEmpty ) {
      actions = Random.shuffle(getActions)
    }
    val action = actions(children.size)
    children +:= action
    action
  }

  def getParent: QState = qParent

  def getActions = {
    val qState = this
    var actions = List[QState]()
    state.getExpandedNodes.forEach(new Consumer[Node] {
      override def accept(t: Node): Unit = actions +:= new QState(t, qState)
    })
    actions
  }

  def update(rewards :List[Float]): Unit = {
    reward += rewards.head // Should contain agent ids but this can be expanded with another node.
    numVisits += 1
  }

  def isFullyExpanded: Boolean = children.nonEmpty && children.size == actions.size

  @inline
  final def isGoal: Boolean = state.isGoalState

  override def toString: String =
    s"Instance:\n${state.toString}"

  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[QState]
  }

  override def equals(other: Any): Boolean = other match {
    case that: QState =>
      if ( !(that canEqual this) ) return false
      val otherNode: Node = that.state
      state.getAgent.equals(otherNode.getAgent) && state.boxes.equals(otherNode.boxes)
    case _ => false
  }

  override def hashCode(): Int = {
    val prime: Int = 31
    var result: Int = 1
    result = prime * result + state.getAgent.hashCode
    result = prime * result + state.boxes.hashCode
    result
  }
}

object QState {
  def getExpandedActions(node: Node): List[QAction] = {
    var expandedNodes: List[QAction] = List[QAction]()
    Command.every.foreach { case c =>
      val newAgentRow: Int = node.getAgent.getPosition.getY + dirToRowChange(c.dir1)
      val newAgentCol: Int = node.getAgent.getPosition.getX + dirToColChange(c.dir1)
      if ( c.actType eq Command.`type`.Move ) {
        if ( cellIsFree(node, newAgentCol, newAgentRow) ) {
          expandedNodes +:= QAction(c)
        }
      } else if ( c.actType eq Command.`type`.Push ) {
        if ( boxAt(node, newAgentCol, newAgentRow) ) {
          val newBoxRow: Int = newAgentRow + dirToRowChange(c.dir2)
          val newBoxCol: Int = newAgentCol + dirToColChange(c.dir2)
          if ( cellIsFree(node, newBoxCol, newBoxRow) ) {
            expandedNodes +:= QAction(c)
          }
        }
      } else if ( c.actType eq Command.`type`.Pull ) {
        if ( cellIsFree(node, newAgentCol, newAgentRow) ) {
          val boxRow: Int = node.getAgent.getPosition.getY + dirToRowChange(c.dir2)
          val boxCol: Int = node.getAgent.getPosition.getX + dirToColChange(c.dir2)
          if ( boxAt(node, boxCol, boxRow) ) {
            expandedNodes +:= QAction(c)
          }
        }
      }
    }
    expandedNodes
  }

  private def boxAt(node: Node, col: Int, row: Int): Boolean = {
    val pos = new Position(col, row)
    node.boxes.exists(box => box.getPosition.equals(pos))
  }

  private def cellIsFree(node: Node, col: Int, row: Int): Boolean = {
    val pos: Position = new Position(col, row)
    !Node.walls.contains(pos) && !boxAt(node, col, row)
  }

  private def dirToRowChange(d: Command.dir): Int = {
    if ( d eq dir.S ) 1
    else if ( d eq dir.N ) -1
    else 0
  }

  private def dirToColChange(d: Command.dir): Int = {
    if ( d eq dir.E ) 1
    else if ( d eq dir.W ) -1
    else 0
  }
}
