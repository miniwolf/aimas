(define (problem projprob1) (:domain PROJECT)
  (:objects
    a0 - agent
    cell1 cell2 cell3 cell4 cell5 - cell
    G1 - goal
    B1 B2 - box
    a A - letter )
  (:init
    (GoalAt G1 cell1)
    (Letter G1 a) (Letter B1 A) (Letter B2 A)
    (Neighbour cell1 cell2) (Neighbour cell2 cell1)
    (Neighbour cell2 cell3) (Neighbour cell3 cell2)
    (Neighbour cell2 cell4) (Neighbour cell4 cell2)
    (Neighbour cell2 cell5) (Neighbour cell5 cell2)
    (Box B1)
    (Box B2)
    (BoxAt B1 cell4)
    (BoxAt B2 cell5)
    (Free cell2) (Free cell1)
    (AgentAt a0 cell3) )

  (:goal (forall (?g1 - goal)
    (exists (?b1 - box ?c - cell ?l - letter)
      (and
        (GoalAt ?g1 ?c) (Letter ?g1 ?l)
        (BoxAt ?b1 ?c) (Letter ?b1 ?l)))))
)
