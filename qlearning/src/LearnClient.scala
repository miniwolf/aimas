import java.io.{BufferedReader, InputStreamReader}
import java.util.Optional
import java.util.function.{Consumer, Predicate, ToIntFunction}

import searchclient.{Node, Position, SearchClient}

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

    val learnClient = new LearnClient(serverMessages, client)
    val Q = QLearner.learn(learnClient, Map.empty, 0.0f, 10000)
    val lookup: (QState) => (QAction) => Float = learnClient.lookupFunction(Q)

    def findWayOut(currentState: QState, actionsSoFar: List[QAction]): List[QAction] = {
      if ( currentState.isGoal ) {
        actionsSoFar
      } else {
        val action = {
          QState.getExpandedActions(currentState.instance) match {
            case Nil => sys.error("No actions available")
            case hd::tl => QLearner.getActionGreedy(lookup, currentState, tl, hd)
          }
        }
        val newActionList = action::actionsSoFar
        val nextState = learnClient.performAction(currentState, action)
        findWayOut(nextState, newActionList)
      }
    }

    val startTime: Long = System.currentTimeMillis
    val route = findWayOut(learnClient.startState, List())
    System.err.println(s"Time: ${System.currentTimeMillis - startTime} ms")
    val solution = route.reverse
    System.err.println("Found solution of length " + solution.size)
  }
}

class LearnClient(serverMessage: BufferedReader, searchClient: SearchClient) extends GameConfiguration {
  val tRandom = new Random()
  val startState = new QState(searchClient.initialState, 3)

  override def performAction(state: QState, action: QAction): QState = {
    val resultState: Optional[Node] = state.instance.getExpandedNodes.stream().filter(new Predicate[Node] {
      override def test(t: Node): Boolean = t.action.equals(action.command)
    }).findFirst()
    resultState match {
      case x if x.isPresent => new QState(x.get(), state.reward)
      case _ => sys.error(s"Illegal action taken by learning step at state: ${state.toString}")
    }
  }

  override def getStartState: QState = startState

  override def lookupFunction(Q: Map[(QState, QAction), Float]) (state: QState) (action: QAction): Float = {
    if ( Q.contains((state, action)) ) {
      Q((state, action))
    } else {
      0.0f
    }
  }

  override def rewardFunction(state: QState) = {
    if ( QState.getExpandedActions(state.instance).size == 1 ) 0.0f
    else if ( state.isGoal ) 100.0f else boxAtGoals(state)
  }

  def boxAtGoals(state: QState): Int = {
    var i = 0
    Node.goals.keySet().forEach(new Consumer[Position] {
      override def accept(t: Position): Unit = {
        val goalC = Node.goals.get(t)
        val boxC = state.instance.boxes.get(t)
        if ( boxC != null && goalC == boxC ) {
          i += 1
        }
      }
    })
    i
  }

  override def alpha: Float = 0.01f // Learning factor

  override def calcEpsilon(x: Float): Float = -0.0001f * x + 1.0f

  override def neutralAction: QAction = QAction(null)

  override def gamma: Float = 0.99f // Discount factor

  override def random: Random = tRandom

  override def serverMessages: BufferedReader = serverMessage
}
