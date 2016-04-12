;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project domain problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain PROJECT)
  (:requirements :typing)
  (:types
    agent
    cell
  )
  (:predicates
    (AgentAt ?a - agent ?c - cell)
    (Free ?c - cell)
    (Neighbour ?a - cell ?b - cell)
  )

  (:action Move :parameters (?x - agent ?y - cell ?z - cell)
   :precondition (and (AgentAt ?x ?y) (Free ?z) (Neighbour ?y ?z))
   :effect (and
    (not (AgentAt ?x ?y)) (AgentAt ?x ?z)
    (not (Free ?z)) (Free ?y) ))
)
