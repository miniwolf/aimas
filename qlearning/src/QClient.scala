import java.io.{BufferedReader, InputStreamReader}
import java.util.Optional
import java.util.function.{Consumer, Predicate, ToIntFunction}

import searchclient.{Box, Node, Position, SearchClient}

import scala.collection.JavaConversions._
import scala.util.Random

/**
  * Created by miniwolf on 19-03-2016.
  */
object QClient extends App {
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

    val learnClient = new QClient(serverMessages, client)
    val Q = QLearner.learn(learnClient, Map.empty, 0, 10000)
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

  def doModified(alpha: Float, gamma: Float, lookup: QState => QAction => Float,
                 isDone: Boolean, currentState: QState, currentAction: QAction): Float = {
    val qsap = if ( isDone ) 0.0f else lookup(currentState)(currentAction)
    val newQsa = alpha * (currentState.reward + gamma * qsap)
    newQsa
  }

  def useInitialPath(client: SearchClient, serverMessages: BufferedReader, solution: List[Node]): Unit = {
    val gc = new QClient(serverMessages, client)
    var Q: Map[(QState, QAction), Float] = Map[(QState, QAction), Float]()
    def iterate(current: QState, currentAction: QAction, isDone: Boolean, reward: Int): Unit = {
      current.instance.parent match {
        case null =>
        case par =>
          val previous = new QState(par, reward)
          val previousAction = QAction(par.action)
          val re = doModified(gc.alpha, gc.gamma, gc.lookupFunction(Q), isDone, current, currentAction)
          Q += ((previous, previousAction) -> re)
          iterate(previous, previousAction, isDone = false, 0)
      }
    }

    val current = new QState(solution.last, 100)
    val currentAction = QAction(solution.last.action)
    Q += ((current, currentAction) -> 100)
    iterate(current, currentAction, isDone = true, reward = 100)

    Q = QLearner.learn(gc, Q, 0, 4)

    def findWayOut(currentState: QState, actionsSoFar: List[QAction]): List[QAction] = {
      if ( currentState.isGoal ) {
        actionsSoFar
      } else {
        val action = QState.getExpandedActions(currentState.instance) match {
          case Nil => sys.error("No actions available")
          case hd::tl => QLearner.getActionGreedy(gc.lookupFunction(Q), currentState, tl, hd)
        }
        val newActionList = action::actionsSoFar
        val nextState = gc.performAction(currentState, action)
        findWayOut(nextState, newActionList)
      }
    }

    val route = findWayOut(gc.startState, List())
    val s = route.reverse
    s.foreach { case n: QAction =>
      val act = n.command.toActionString
      println(act)
      val response: String = serverMessages.readLine
      if ( response.contains("false") ) {
        System.err.println(s"Server responded with $response to the inapplicable action: $act\n")
        System.err.format(s"$act was attempted in \n")
        return
      }
    }
  }
}

class QClient(serverMessage: BufferedReader, searchClient: SearchClient) extends GameConfiguration {
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
        val boxes: List[Box] = state.instance.boxes.filter(box => box.getPosition.equals(t)).toList
        boxes match {
          case (car: Box) :: cdr if goalC == Character.toLowerCase(car.getCharacter) => i += 1
          case _ => i = i
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
