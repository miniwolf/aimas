import FibonacciHeap.Entry
import searchclient.Position

import scala.collection.mutable.HashSet
import scala.collection.immutable
/**
  * Created by miniwolf on 08-04-2016.
  */
object Astar {
  def heuristic(start: Position, goal: Position) = {
    Math.abs(start.getX - goal.getX) + Math.abs(start.getY - goal.getY)
  }

  class Node(val parent: Node, val position: Position, val gValue: Int) {
    def extractPath(currentPath: List[Position]): List[Position] = {
      parent match {
        case null => currentPath.reverse
        case _ => parent.extractPath(position :: currentPath)
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

    override def equals(other: Any): Boolean = other match {
      case that: Node => (that canEqual this) && position == that.position
      case _ => false
    }
  }

  def search(edges: Map[Position, List[Position]], start: Position, goal: Position): List[Position] = {
    val eKeys = edges.keySet
    if ( !eKeys.contains(start) || !eKeys.contains(goal) ) {
      return List()
    }
    val startNode = new Node(null, start, 0)
    val openSet = new FibonacciHeap[Node]()
    var closedSet = new HashSet[Position]()
    var openEntryMap = Map[Node, Entry[Node]]()
    openEntryMap += (startNode -> openSet.enqueue(startNode, heuristic(start, goal)))

    while ( !openSet.isEmpty ) {
      val leafNode: Node = openSet.dequeueMin().getValue
      if ( leafNode.position.equals(goal) ) {
        return leafNode.extractPath(List())
      }

      closedSet += leafNode.position
      edges(leafNode.position).filter(n => !closedSet.contains(n)
                                           && !openEntryMap.keySet.contains(new Node(null, n, 0)))
                              .foreach { case child =>
        val childNode = new Node(leafNode, child, leafNode.gValue + 1)
        openEntryMap += (childNode -> openSet.enqueue(childNode, childNode.gValue + heuristic(child, goal)))
      }
    }
    List[Position]()
  }

  def search2(edges: Map[Position, List[Position]], start: Position, goal: Position, boxSet: HashSet[Position]): List[Position] = {
    val eKeys = edges.keySet
    if ( !eKeys.contains(start) || !eKeys.contains(goal) ) {
      return List()
    }
    val startNode = new Node(null, start, 0)
    val openSet = new FibonacciHeap[Node]()
    var closedSet = new HashSet[Position]()
    var openEntryMap = Map[Node, Entry[Node]]()
    openEntryMap += (startNode -> openSet.enqueue(startNode, heuristic(start, goal)))

    while ( !openSet.isEmpty ) {
      val leafNode: Node = openSet.dequeueMin().getValue
      if ( leafNode.position.equals(goal) ) {
        return leafNode.extractPath(List())
      }

      closedSet += leafNode.position
      edges(leafNode.position).filter(n => !closedSet.contains(n)
        && !openEntryMap.keySet.contains(new Node(null, n, 0)))
        .foreach { case child =>
          val pathCost = if ( boxSet.contains(child) ) 2 else 1
          val childNode = new Node(leafNode, child, leafNode.gValue + pathCost)
          openEntryMap += (childNode -> openSet.enqueue(childNode, childNode.gValue + heuristic(child, goal)))
        }
    }
    List[Position]()
  }

  def search3(edges: Map[Position, List[Position]], start: Position, path: immutable.HashSet[Position]): Position = {
    val eKeys = edges.keySet
    if ( !eKeys.contains(start)) {
      return null
    }
    val startNode = new Node(null, start, 0)
    val openSet = new FibonacciHeap[Node]()
    var closedSet = new HashSet[Position]()
    var openEntryMap = Map[Node, Entry[Node]]()
    openEntryMap += (startNode -> openSet.enqueue(startNode, 0))

    while ( !openSet.isEmpty ) {
      val leafNode: Node = openSet.dequeueMin().getValue
      if ( !path.contains(leafNode.position) ) {
        return leafNode.position
      }

      closedSet += leafNode.position
      edges(leafNode.position).filter(n => !closedSet.contains(n)
        && !openEntryMap.keySet.contains(new Node(null, n, 0)))
        .foreach { case child =>
          val childNode = new Node(leafNode, child, leafNode.gValue + 1)
          openEntryMap += (childNode -> openSet.enqueue(childNode, childNode.gValue))
        }
    }
    null
  }
}