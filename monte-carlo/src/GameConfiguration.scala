import scala.util.Random

/**
  * Created by miniwolf on 18-03-2016.
  */
trait GameConfiguration {
  def kValue: Float
  def simulationDepth: Int
  def random: Random
  def evaluate(state: QState): List[Float]
  def maxTime: Int
  def maxIterations: Int
}
