import java.util.InputMismatchException

import searchclient.SearchClient.Memory

import scala.util.Random

/**
  * Created by miniwolf on 18-03-2016.
  */
object QLearner {
  def doAction(performAction: (QState, QAction) => QState, rewardFunction: (QState) => Float, state: QState, action: QAction) = {
    val newState = performAction(state, action)
    val reward = rewardFunction(newState)
    val resState = QState(newState.instance, reward)
    (resState, state.actions)
  }

  def doLearningStep(alpha: Float, gamma: Float, lookup: QState => QAction => Float,
                     isDone: Boolean, previousState: QState, previousAction: QAction,
                     currentState: QState, currentAction: QAction): (QState, QAction, Float) = {
    val qsa: Float = lookup(previousState)(previousAction)
    val qsap = if ( isDone ) 0.0f else lookup(currentState)(currentAction)
    val newQsa = qsa + alpha * (currentState.reward + gamma * qsap - qsa)
    (previousState, previousAction, newQsa)
  }

  def getActionGreedy(lookup: QState => QAction => Float, currentState: QState, actions: List[QAction], bestActionSoFar: QAction): QAction = {
    val currentBestValue = lookup(currentState)(bestActionSoFar)
    actions match {
      case Nil => bestActionSoFar
      case hd :: tl =>
        val currentActionValue = lookup(currentState)(hd)
        currentActionValue match {
          case x if x > currentBestValue => getActionGreedy(lookup, currentState, tl, hd)
          case _ => getActionGreedy(lookup, currentState, tl, bestActionSoFar)
        }
    }
  }

  def getRandomAction(random: Random, actions: List[QAction]): Option[QAction] = {
    actions match {
      case List() => None
      case _ => Some(actions(random.nextInt(actions.length)))
    }
  }

  def getActionEGreedy(random: Random, lookup: QState => QAction => Float, actions: List[QAction], epsilon: Float, currentState: QState): Option[QAction] = {
    actions match {
      case Nil => None
      case car :: cdr =>
        random.nextDouble() match {
          case x if x > epsilon => Some(getActionGreedy(lookup, currentState, cdr, car))
          case _ => getRandomAction(random, actions)
        }
    }
  }

  def playOneRound(gc: GameConfiguration, Q: Map[(QState, QAction), Float], epsilon: Float, history: List[(QState, QAction)], currentState: QState): Map[(QState, QAction), Float] = {
    val isDone = currentState.isGoal
    val lookup: QState => QAction => Float = gc.lookupFunction(Q)
    val action = {
      val foundAction = getActionEGreedy(gc.random, lookup, currentState.actions, epsilon, currentState)
      foundAction match {
        case None => gc.neutralAction
        case Some(x) => x
      }
    }
    val (_, newQ) = {
      history match {
        case List() => (0.0f, Q.+((currentState, action) -> 0.0f))
        case (prevState, prevAction)::tl =>
          val (_,_, re) = doLearningStep(gc.alpha, gc.gamma, lookup, isDone, prevState, prevAction, currentState, action)
          val nq = Q.+((prevState, prevAction) -> re)
          (re, nq)
      }
    }

    if ( isDone ) {
      newQ
    } else {
      val newHistory = (currentState, action) :: history
      val (newState, _) = doAction(gc.performAction, gc.rewardFunction, currentState, action)
      playOneRound(gc, newQ, epsilon, newHistory, newState)
    }
  }

  def learn(gc: GameConfiguration, Q: Map[(QState, QAction), Float], counter: Float, roundsLeft: Int): Map[(QState, QAction), Float] = {
    roundsLeft match {
      case 0 => Q
      case x if x > 0 =>
        val startState: QState = gc.getStartState
        val newCounter = counter + 1.0f
        val epsilon = gc calcEpsilon newCounter
        val newQ = playOneRound(gc, Q, epsilon, List(), startState)
        if ( roundsLeft % 200 == 0 ) {
          println(s"rounds left: $roundsLeft - ${Memory.stringRep()}")
        }
        learn(gc, newQ, newCounter, roundsLeft - 1)
      case _ => throw new InputMismatchException("Cannot handle negative rounds left")
    }
  }
}
