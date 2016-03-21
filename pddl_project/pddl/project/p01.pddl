(define (problem projprob1) (:domain PROJECT)
  (:objects
    a0 - agent
    cell1 cell2 cell3 cell4 - cell
    G1 - goal
    B1 - box
    a A - letter )
  (:init
    (GoalAt G1 cell1)
    (Letter G1 a) (Letter B1 A)
    (Neighbour cell1 cell2) (Neighbour cell2 cell1)
    (Neighbour cell2 cell3) (Neighbour cell3 cell2)
    (Neighbour cell2 cell4) (Neighbour cell4 cell2)
    (BoxAt B1 cell4)
    (Free cell2) (Free cell1)
    (AgentAt a0 cell3) )

  (:goal (and
    (GoalAt G1 cell1)
    (BoxAt B1 cell1)
    (Letter B1 A)
    (Letter G1 a) ))
)
