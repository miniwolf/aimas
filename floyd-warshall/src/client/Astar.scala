package client

import client.FibonacciHeap.Entry
import core.Position

import scala.collection.immutable.HashSet

/**
  * @author miniwolf and Maciej
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

  def search(edges: Map[Position, List[Position]], start: Position, goal: Position, boxSet: HashSet[Position]): List[Position] = {
    val eKeys = edges.keySet
    if ( !eKeys.contains(start) || !eKeys.contains(goal) ) {
      return List()
    }
    val startNode = new Node(null, start, 0)
    val openSet = new FibonacciHeap[Node]()
    val closedSet = new HashSet[Position]()
    var openEntryMap = Map[Node, Entry[Node]]()
    openEntryMap += (startNode -> openSet.enqueue(startNode, heuristic(start, goal)))

    def searchInternal(openEntryMap: Map[Node, Entry[Node]], closedSet: HashSet[Position],
                       openSet: FibonacciHeap[Node], edges: Map[Position, List[Position]],
                       goal: Position): List[Position] = {
      def addChildren(children: List[Position], openEntryMap: Map[Node, Entry[Node]], leafNode: Node,
                      openSet: FibonacciHeap[Node], goal: Position, boxSet: HashSet[Position]): Map[Node, Entry[Node]] = {
        children match {
          case Nil => openEntryMap
          case child :: cdr =>
            val pathCost = if ( boxSet.contains(child) ) { 2 } else { 1 }
            val childNode = new Node(leafNode, child, leafNode.gValue + pathCost)
            val newOpenEntryMap = openEntryMap + (childNode -> openSet.enqueue(childNode, childNode.gValue + heuristic(child, goal)))
            addChildren(cdr, newOpenEntryMap, leafNode, openSet, goal, boxSet)
        }
      }

      openSet match {
        case _ if openSet.isEmpty => List[Position]()
        case _ =>
          openSet.dequeueMin().getValue match {
            case leaf if leaf.position.equals(goal) => leaf.extractPath(List())
            case leafNode =>
              val newClosedSet = closedSet + leafNode.position
              val children = edges(leafNode.position).filter(n => !closedSet.contains(n)
                && !openEntryMap.keySet.contains(new Node(null, n, 0)))
              val newOpenEntryMap = addChildren(children, openEntryMap, leafNode, openSet, goal, boxSet)
              searchInternal(newOpenEntryMap, newClosedSet, openSet, edges, goal)
          }
      }
    }
    searchInternal(openEntryMap, closedSet, openSet, edges, goal)
  }

  def search3(edges: Map[Position, List[Position]], start: Position, path: HashSet[Position], depth: Int): Position = {
    val eKeys = edges.keySet
    if ( !eKeys.contains(start) ) {
      return null
    }

    var foundNodes = List[Position]()
    val startNode = new Node(null, start, 0)
    val openSet = new FibonacciHeap[Node]()
    var closedSet = new HashSet[Position]()
    var openEntryMap = Map[Node, Entry[Node]]()
    openEntryMap += (startNode -> openSet.enqueue(startNode, 0))

    while ( !openSet.isEmpty ) {
      val leafNode: Node = openSet.dequeueMin().getValue
      if ( !path.contains(leafNode.position) ) {
        if ( foundNodes.size < depth ) {
          foundNodes = leafNode.position :: foundNodes
        } else {
          return leafNode.position
        }
      }

      closedSet += leafNode.position
      edges(leafNode.position).filter(n => !closedSet.contains(n)
                                        && !openEntryMap.keySet.contains(new Node(null, n, 0)))
                              .foreach { case child =>
        val childNode = new Node(leafNode, child, leafNode.gValue + 1)
        openEntryMap += (childNode -> openSet.enqueue(childNode, childNode.gValue))
      }
    }
    if ( foundNodes.nonEmpty ) {
      foundNodes.head
    } else {
      null
    }
  }
}
