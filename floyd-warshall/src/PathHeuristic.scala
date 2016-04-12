import java.util.Comparator

import searchclient.{Node, Position}

/**
  * @author miniwolf
  */
object PathHeuristic {
  class AStar(goalState: Position) extends PathHeuristic(goalState) {
    def f(n: Node) = n.g + h(n)
  }
}

abstract class PathHeuristic(goalState: Position) extends Comparator[Node] {
  def compare(n1: Node, n2: Node): Int = f(n1) - f(n2)

  def h(n: Node): Int = {
    val pos = n.getAgent.getPosition
    Math.abs(pos.getX - goalState.getX) + Math.abs(pos.getY - goalState.getY)
  }

  def f(n: Node): Int
}
