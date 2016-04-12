import java.io.{BufferedReader, InputStreamReader}
import java.util.Optional
import java.util.function.{Consumer, Predicate, ToIntFunction}

import searchclient.{Box, Node, Position, SearchClient}

import scala.util.Random

/**
  * Created by miniwolf on 19-03-2016.
  */
object LearnClient extends App {
  @throws[Exception]
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

    val learnClient = new LearnClient(client)
    var Q: QState = learnClient.startState
    while ( !Q.isGoal ) {
      Q = MonteSearch.estimateAction(learnClient, Q)
      val act = Q.state.action.toActionString
      println(act)
      val response: String = serverMessages.readLine
      if ( response.contains("false") ) {
        System.err.format("Server responded with %s to the inapplicable action: %s\n", response, act)
        System.err.format("%s was attempted in \n", act)
        return
      }
    }
    /*while ( !Q.isGoal ) {
      Q = MonteSearch.estimateAction(learnClient, Q)
    }
    println(s"res: ${Memory.stringRep()}")
    def findWayOut(currentState: QState, actionsSoFar: List[QAction]): List[QAction] = {
      if ( currentState.getParent == null ) {
        actionsSoFar
      } else {
        val action = QAction(currentState.state.action)
        val newActionList = action :: actionsSoFar
        findWayOut(currentState.getParent, newActionList)
      }
    }

    val startTime: Long = System.currentTimeMillis
    val route = findWayOut(Q, List())
    System.err.println(s"Time: ${System.currentTimeMillis - startTime} ms")
    System.err.println("Found solution of length " + route.size)

    route.foreach { case n: QAction =>
      val act: String = n.command.toActionString
      System.out.println(act)
      val response: String = serverMessages.readLine
      if ( response.contains("false") ) {
        System.err.format("Server responded with %s to the inapplicable action: %s\n", response, act)
        System.err.format("%s was attempted in \n%s\n", act, n)
        return
      }
    }*/
  }
}

class LearnClient(searchClient: SearchClient) extends GameConfiguration {
  val tRandom = new Random()
  val startState = new QState(searchClient.initialState, null)
  val kV = Math.sqrt(2)
  override def random: Random = tRandom

  override def kValue: Float = kV.toFloat

  override def maxTime: Int = 100

  override def simulationDepth: Int = 10000

  override def evaluate(state: QState): List[Float] = {
    List(if ( state.isGoal ) 10
    else boxAtGoals(state))
  }

  def boxAtGoals(state: QState): Int = {
    var i = 0
    Node.goals.keySet().forEach(new Consumer[Position] {
      override def accept(pos: Position): Unit = {
        val goalC = Node.goals.get(pos)
        val b: Optional[Box] = state.boxes.stream.filter(new Predicate[Box] {
          override def test(box: Box): Boolean = box.getPosition.equals(pos)
        }).findFirst

        if ( b.isPresent && goalC == b.get().getCharacter ) {
          i += 1
        }
      }
    })
    i
  }

  override def maxIterations: Int = 1000
}
