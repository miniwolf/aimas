package client

import client.FibonacciHeap.Entry
import core.Position

/**
  * @author miniwolf
  */
object Dijkstra {
  def getPath(prev: Map[Position, Option[Position]], to: Position): List[Position] = {
    var S = List[Position](to)
    var u = to
    var oddElement: Position = null
    var element: Position = null
    while ( prev(u).isDefined ) {
      u = prev(u).get
      if ( S.length % 2 == 0 ) {
        if ( u.equals(element) ) {
          return null
        } else {
          element = u
        }
      } else {
        if ( u.equals(oddElement) ) {
          return null
        } else {
          oddElement = u
        }
      }
      S = u :: S
    }
    S
  }

  def computeDistances(vertices: List[Position], edges: List[(Position, Position)], from: Position, to: Position): List[Position] = {
    if ( !vertices.contains(from) || !vertices.contains(to) ) {
      return null
    }
    val Q = new FibonacciHeap[Position]()
    var entryMap = Map[Position, Entry[Position]]()
    var prev = Map[Position, Option[Position]]()
    var dist = Map[Position, Int]()
    dist += (from -> 0)

    vertices.foreach { case v =>
      if ( v != from ) {
        dist += (v -> Int.MaxValue)
      }
      prev += (v -> None)
      entryMap += (v -> Q.enqueue(v, dist(v)))
    }

    while ( !Q.isEmpty ) {
      val u: Entry[Position] = Q.dequeueMin()
      if ( u.getValue.equals(to) ) {
        return getPath(prev, to)
      }

      edges.filter(p => p._1.equals(u.getValue)).foreach { case (_, v) =>
        val alt = dist(u.getValue) + 1 // One will always be the length to a neighbour
        if ( alt < dist(v) ) {
          dist += (v -> alt)
          prev += (v -> Some(u.getValue))
          Q.decreaseKey(entryMap(v), alt)
        }
      }
    }
    null
  }
}
