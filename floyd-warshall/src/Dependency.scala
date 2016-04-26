import searchclient.{Node, Position}

import scala.collection.immutable.Map
import scala.collection.JavaConversions._

/**
  * Created by miniwolf on 26-04-2016.
  */
object Dependency {
  def getGoalDependencies(permutations: List[Position], goalMatch: Map[Position, Int],
                          initialState: Node): (Map[Position, List[Position]], Map[Position, Int]) = {
    var goalMatches: Map[Position, Int] = goalMatch
    def calcDependency(goalDependencies: Map[Position, List[Position]],
                       goals: List[Position], restarted: Boolean): (Map[Position, List[Position]], Map[Position, Int]) = {
      goals match {
        case Nil => (goalDependencies, goalMatches)
        case goal :: cdr =>
          val (dependencies, newGoalMatches, hasChanged) = getGoalDependency(goal, permutations, goalMatches, initialState)
          if ( hasChanged ) {
            if ( restarted ) {
              sys.error("Possible circular dependency on character of the same letter")
            }
            goalMatches = newGoalMatches
            calcDependency(Map(), permutations, restarted = true)
          } else {
            calcDependency(goalDependencies + (goal -> dependencies), cdr, restarted = false)
          }
      }
    }
    if ( permutations.size == 1 ) {
      (Map(permutations.head -> List()), goalMatches)
    } else {
      calcDependency(Map(), permutations, false)
    }
  }

  def getGoalDependency(permutation: Position, goals: List[Position], goalMatch: Map[Position, Int],
                        initialState: Node): (List[Position], Map[Position, Int], Boolean) = {
    Node.walls.add(permutation)
    val emptyStartState = new Node(initialState.parent)
    emptyStartState.setAgent(initialState.getAgent)
    val (_, edges) = Graph.construct(emptyStartState)

    def findDependencies(dependencies: List[Position], goals: List[Position]): List[Position] = {
      goals match {
        case Nil => dependencies
        case car :: cdr =>
          Astar.search(edges, car,
                       initialState.boxes.find(box => box.getId == goalMatch(car)).get.getPosition) match {
            case null | Nil => findDependencies(car :: dependencies, cdr)
            case _ => findDependencies(dependencies, cdr)
          }
      }
    }
    val dependencies = findDependencies(List(), goals.filter(goal => !goal.equals(permutation)))
    Node.walls.remove(permutation)
    val goalChar = Node.goals.get(permutation)
    if ( dependencies.length == 1 ) {
      val matchingGoal = dependencies.find(goal => Node.goals.get(goal).equals(goalChar))
      matchingGoal match {
        case None => (dependencies, goalMatch, false)
        case Some(goal) =>
          val (goalDependencies, _, _) = getGoalDependency(goal, goals, goalMatch, initialState)
          if ( goalDependencies.isEmpty ) {
            (dependencies, goalMatch, false)
          } else {
            val boxIdx = goalMatch.get(permutation).get
            val newBoxIdx = goalMatch.get(goal).get
            var newGoalMatch = goalMatch.filter { case (goalPos, id) => (!permutation.equals(goalPos) && id != boxIdx) ||
              (!goal.equals(goalPos) && id != newBoxIdx) }
            newGoalMatch = newGoalMatch + (permutation -> newBoxIdx) + (goal -> boxIdx)
            (dependencies, newGoalMatch, true)
          }
      }
    } else {
      (dependencies, goalMatch, false)
    }
  }
}
