import scala.util.Random

/**
  * Created by miniwolf on 18-03-2016.
  */
trait GameConfiguration {
  def performAction(state: QState, action: QAction): QState
  def rewardFunction(state: QState): Float
  def lookupFunction(Q: Map[(QState, QAction), Float]) (state: QState) (action: QAction): Float
  def calcEpsilon(float: Float): Float
  def random: Random
  def neutralAction: QAction
  def getStartState: QState
  def alpha: Float
  def gamma: Float
}
