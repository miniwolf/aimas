package client.heuristic

import java.util.Comparator

import searchclient.Node

/**
  * Created by miniwolf on 03-05-2016.
  */
trait Heuristic extends Comparator[Node] {
  def h(n: Node): Int

  def f(n: Node) = n.g + h(n)
  def compare(n1: Node, n2: Node): Int = f(n1) - f(n2)

  override def toString = "A* evaluation"
}
