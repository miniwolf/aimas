package client.singleagent

import java.io.{BufferedReader, InputStreamReader}
import java.util.function.ToIntFunction

import client.{Graph, PathFinding, Solution}
import core.singleagent.{SingleAgent, SingleNode}
import core.{Box, Node, Position}
import searchclient.SearchClient.Memory
import searchclient._

import scala.collection.JavaConversions._
import scala.collection.immutable.Map

/**
  * @author miniwolf
  */
object LearnClient extends App {
  override def main(args: Array[String]) {
    val serverMessages: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    System.err.println("SearchClient initializing. I am sending this using the error output stream.")

    val lines: java.util.List[String] = new java.util.ArrayList[String]
    var line: String = serverMessages.readLine
    while ( !line.equals("") ) {
      lines.add(line)
      line = serverMessages.readLine
    }
    Node.MAX_COLUMN = lines.stream().mapToInt(new ToIntFunction[String] {
      override def applyAsInt(value: String): Int = value.length
    }).max.getAsInt
    Node.MAX_ROW = lines.size
    val client: SearchClient = new SearchClient(lines)
    val startTime = System.currentTimeMillis()
    val learnClient = new LearnClient(client)
    val (_, edges) = Graph.construct(learnClient.emptyStartState)
    val goalMatches = matchGoalsWithBoxes(learnClient.startState, edges)
    val goalList = Node.goals.map { case (goalPos, goalChar) => goalPos -> goalChar }.toMap

    val solution: List[Node] = Solution.findSolution(goalList.keys.toList, goalMatches, List(), List(),
                                                           learnClient.startState)
    val timeSpent = (System.currentTimeMillis - startTime) / 1000f
    System.err.println(s"Summary: Time: $timeSpent\t ${Memory.stringRep()}")
    Node.walls.removeAll(goalList.keys.toList)
    Node.goals = Node.goals ++ goalList
    solution.foreach { case n: Node =>
      val act = n.getAction
      println(act)
      val response: String = serverMessages.readLine
      if ( response.contains("false") ) {
        System.err.println(s"Server responded with $response to the inapplicable action: $act\n")
        System.err.format(s"$act was attempted in \n")
        return
      }
    }
  }

  def test(lines: List[String]) = {
    System.err.println("SearchClient initializing. I am sending this using the error output stream.")

    Node.MAX_COLUMN = lines.stream().mapToInt(new ToIntFunction[String] {
      override def applyAsInt(value: String): Int = value.length
    }).max.getAsInt
    Node.MAX_ROW = lines.size
    val client: SearchClient = new SearchClient(lines)
    val startTime = System.currentTimeMillis()
    val learnClient = new LearnClient(client)
    val (_, edges) = Graph.construct(learnClient.emptyStartState)
    val goalMatches = matchGoalsWithBoxes(learnClient.startState, edges)
    val goalList = Node.goals.map { case (goalPos, goalChar) => goalPos -> goalChar }.toMap

    val solution: List[Node] = Solution.findSolution(goalList.keys.toList, goalMatches, List(), List(),
                                                           learnClient.startState)
    val timeSpent = (System.currentTimeMillis - startTime) / 1000f
    println(s"Summary: Time: $timeSpent\t ${Memory.stringRep()}")
    println(s"SolutionLength: ${solution.length}")
  }

  def matchGoalsWithBoxes(initialState: Node, edges: Map[Position, List[Position]]): Map[Position, Int] = {
    var goalMatch = Map[Position, Int]() // Goal position, Box
    Node.goals.foreach { case (goalPos, goalChar) =>
      def getBestBox(currentBest: Box, boxes: List[Box]): Int = {
        boxes match {
          case Nil =>
            currentBest.setGoalLink(goalPos)
            currentBest.getId
          case box :: cdr if currentBest == null =>
            val currentPath = PathFinding.findPath2(initialState, box, goalPos, edges)
            box.setGoalPath(currentPath)
            getBestBox(box, cdr)
          case box :: cdr =>
            val currentPath = PathFinding.findPath2(initialState, box, goalPos, edges)
            box.setGoalPath(currentPath)
            currentPath.length < currentBest.getGoalPath.length match {
              case true => getBestBox(box, cdr)
              case false => getBestBox(currentBest, cdr)
            }
        }
      }
      val boxes = initialState.getBoxes.filter(box => goalChar.equals(Character.toLowerCase(box.getCharacter))
                                                   && !goalMatch.values.contains(box.getId)).toList
      goalMatch += (goalPos -> getBestBox(null, boxes))
    }
    goalMatch
  }
}

class LearnClient(searchClient: SearchClient) {
  val startState = searchClient.initialState
  val emptyStartState = new SingleNode(startState.parent)
  emptyStartState.setAgent(new SingleAgent(startState.getAgent.getPosition, 0))
}