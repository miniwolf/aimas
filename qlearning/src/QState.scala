import searchclient.Command.dir
import searchclient.{Command, Node, Position}

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

object QState {
  def getExpandedActions(node: Node): List[QAction] = {
    var expandedNodes: List[QAction] = List[QAction]()
    Command.every.foreach { case c =>
      val newAgentRow: Int = node.agentRow + dirToRowChange(c.dir1)
      val newAgentCol: Int = node.agentCol + dirToColChange(c.dir1)
      if ( c.actType eq Command.`type`.Move ) {
        if ( cellIsFree(node, newAgentRow, newAgentCol) ) {
          expandedNodes +:= QAction(c)
        }
      }
      else if ( c.actType eq Command.`type`.Push ) {
        if ( boxAt(node, newAgentRow, newAgentCol) ) {
          val newBoxRow: Int = newAgentRow + dirToRowChange(c.dir2)
          val newBoxCol: Int = newAgentCol + dirToColChange(c.dir2)
          if ( cellIsFree(node, newBoxRow, newBoxCol) ) {
            expandedNodes +:= QAction(c)
          }
        }
      }
      else if ( c.actType eq Command.`type`.Pull ) {
        if ( cellIsFree(node, newAgentRow, newAgentCol) ) {
          val boxRow: Int = node.agentRow + dirToRowChange(c.dir2)
          val boxCol: Int = node.agentCol + dirToColChange(c.dir2)
          if ( boxAt(node, boxRow, boxCol) ) {
            expandedNodes +:= QAction(c)
          }
        }
      }
    }
    expandedNodes
  }

  private def boxAt(node: Node,row: Int, col: Int): Boolean = {
    node.boxes.containsKey(new Position(row, col))
  }

  private def cellIsFree(node: Node, row: Int, col: Int): Boolean = {
    val pos: Position = new Position(row, col)
    !Node.walls.contains(pos) && !node.boxes.containsKey(pos)
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
