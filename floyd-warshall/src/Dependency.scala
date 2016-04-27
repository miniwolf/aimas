import searchclient.{Node, Position}

import scala.collection.JavaConversions._
import scala.collection.immutable.Map

/**
  * Created by miniwolf on 26-04-2016.
  */
object Dependency {
  def fixCircularDependencies(list: List[(Position, List[Position])], goalMatch: Map[Position, Int], needsRestart: Boolean): (Map[Position, Int], Boolean) = {
    list match {
      case Nil => (goalMatch, needsRestart)
      case (goal, dependency) :: cdr if Node.goals.get(goal).equals(Node.goals.get(dependency.head)) =>
        val boxId = goalMatch(goal)
        val newGoalMatch = goalMatch + (goal -> goalMatch(dependency.head)) + (dependency.head -> boxId)
        fixCircularDependencies(cdr, newGoalMatch, needsRestart)
      case car :: cdr => fixCircularDependencies(cdr, goalMatch, needsRestart)
    }
  }

  def getGoalDependencies(permutations: List[Position], goalMatch: Map[Position, Int],
                          initialState: Node): (Map[Position, List[Position]], Map[Position, Int]) = {
    def calcDependency(goalDependencies: Map[Position, List[Position]],
                       goals: List[Position]): Map[Position, List[Position]] = {
      goals match {
        case Nil => goalDependencies
        case goal :: cdr =>
          Node.walls.add(goal)
          val dependencies = getGoalDependency(goal, permutations, goalMatch, initialState)
          Node.walls.remove(goal)
          calcDependency(goalDependencies + (goal -> dependencies), cdr)
      }
    }
    if ( permutations.size == 1 ) {
      (Map(permutations.head -> List()), goalMatch)
    } else {
      val dependencies = calcDependency(Map(), permutations)
      val singleDependencies = dependencies.filter(pair => pair._2.size == 1).toList
      fixCircularDependencies(singleDependencies, goalMatch, needsRestart = false) match {
        case (_, false) => (dependencies, goalMatch)
        case (newGoalMatches, true) => getGoalDependencies(permutations, newGoalMatches, initialState)
      }
    }
  }

  def getGoalDependency(permutation: Position, goals: List[Position], goalMatch: Map[Position, Int],
                        initialState: Node): List[Position] = {
    val emptyStartState = new Node(initialState.parent)
    emptyStartState.setAgent(initialState.getAgent)
    val (_, edges) = Graph.construct(emptyStartState)

    def findDependencies(dependencies: List[Position], goals: List[Position]): List[Position] = {
      goals match {
        case Nil => dependencies
        case goal :: cdr =>
          PathFinding.findPath2(initialState, initialState.boxes.find(box => box.getId == goalMatch(goal)).get, goal, edges) match {
            case null | Nil => findDependencies(goal :: dependencies, cdr)
            case _ => findDependencies(dependencies, cdr)
          }
      }
    }
    findDependencies(List(), goals.filter(goal => !goal.equals(permutation)))
  }
}
