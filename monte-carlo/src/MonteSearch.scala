import scala.util.Random

/**
  * Created by miniwolf on 22-03-2016.
  *
  * kValue in UCT function. default = sqrt(2)
  * simulationDepth number of ticks to run simulation
  */
object MonteSearch {
  val timer = new Timer()

  def estimateAction(gc: GameConfiguration, currentState: QState): QState = {
    timer.start()
    var bestNode: QState = null
    var iterations = 0
    while ( !timer.checkDuration(gc.maxTime) && gc.maxIterations > iterations ) {
      var node: QState = currentState
      while ( !node.isGoal && node.isFullyExpanded ) {
        node = getUCT(node, gc.kValue)
      }

      // 2. EXPANSION adding a child
      if ( !node.isFullyExpanded && !node.isGoal ) {
        node = node.expand
      }

      // 3. SIMULATE
      var state = node
      if ( !node.isGoal ) {
        var action: QState = null
        def simulate(): Unit = {
          var i = 0
          while ( i < gc.simulationDepth ) {
            if ( state.isGoal ) {
              return
            }

            getRandomAction(gc.random, state.getActions) match {
              case None => return
              case Some(x) => state = x
            }
            i += 1
          }
        }
        simulate()
      }

      val rewards: List[Float] = gc.evaluate(state)

      // 4. BACK PROPAGATION
      while ( node != null ) {
        node.update(rewards)
        node = node.getParent
      }

      if ( node != null && node.isGoal ) {
        return node
      }
      bestNode = mostVisited(currentState)
      iterations += 1
    }

    bestNode
  }

  def mostVisited(node: QState): QState = node.children.maxBy(f => f.numVisits)

  def getRandomAction(random: Random, actions: List[QState]): Option[QState] = {
    actions match {
      case List() => None
      case _ => Some(actions(random.nextInt(actions.length)))
    }
  }

  def getUCT(node: QState, uctK: Float): QState = {
    val bestNode = node.children.max(new Ordering[QState] {
      override def compare(x: QState, y: QState): Int = {
        val exploitation_x = x.reward / x.numVisits + 0.000001f
        val exploitation_y = y.reward / y.numVisits + 0.000001f
        val exploration_x = Math.sqrt(Math.log(x.numVisits + 1) / (x.numVisits + 0.000001f))
        val exploration_y = Math.sqrt(Math.log(y.numVisits + 1) / (y.numVisits + 0.000001f))
        val score_x = exploitation_x + uctK * exploration_x
        val score_y = exploitation_y + uctK * exploration_y
        score_x.compare(score_y)
      }
    })
    bestNode
  }
}
