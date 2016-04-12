import searchclient.{Node, Position}

import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 29-03-2016.
  */
object Graph {
  def construct(initialNode: Node): (List[Position], Map[Position, List[Position]]) = {
    construct(initialNode, List(), List(), Map())
  }

  private def construct(node: Node, currentVisiting: List[Node], vertices: List[Position], edges: Map[Position, List[Position]]): (List[Position], Map[Position, List[Position]]) = {
    val agentPos = node.getAgent.getPosition
    val children = node.getExpandedNodes
    val newEdges = edges + (agentPos -> children.map(child => child.getAgent.getPosition).toList)
    val newVertices = agentPos :: vertices
    children.filter(p => !vertices.contains(p.getAgent.getPosition)).toList match {
      case Nil => currentVisiting.filter(p => !newVertices.contains(p.getAgent.getPosition)) match {
        case Nil => (newVertices, newEdges)
        case head :: tail => construct(head, tail, newVertices, newEdges)
      }
      case head :: tail => construct(head, currentVisiting ::: tail, newVertices, newEdges)
    }
  }
}
