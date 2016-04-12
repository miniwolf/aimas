(define (problem projprob1) (:domain PROJECT)
  (:objects
    a - agent
    x y - cell )
  (:init
    (Neighbour x y) (Neighbour y x)
    (Free x)
    (AgentAt a y) )
  (:goal (and
    (AgentAt a x) ))
)
