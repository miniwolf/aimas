package client

import scala.collection.JavaConversions._
import searchclient.{Node, Position}

import scala.collection.immutable.HashSet

/**
  * Created by miniwolf on 29-03-2016.
  */
object Graph {
  def construct(initialNode: Node): (HashSet[Position], Map[Position, List[Position]]) = {
    construct(initialNode, List(), HashSet(), Map())
  }

  private def construct(node: Node, currentVisiting: List[Node], vertices: HashSet[Position],
                        edges: Map[Position, List[Position]]): (HashSet[Position], Map[Position, List[Position]]) = {
    val agentPos = node.getAgent.getPosition
    val children = node.getExpandedNodes
    val newEdges = edges + (agentPos -> children.map(child => child.getAgent.getPosition).toList)
    val newVertices = vertices + agentPos
    children.filter(p => !vertices.contains(p.getAgent.getPosition)).toList match {
      case Nil => currentVisiting.filter(p => !newVertices.contains(p.getAgent.getPosition)) match {
        case Nil => (newVertices, newEdges)
        case head :: tail => construct(head, tail, newVertices, newEdges)
      }
      case head :: tail => construct(head, currentVisiting ::: tail, newVertices, newEdges)
    }
  }
}
