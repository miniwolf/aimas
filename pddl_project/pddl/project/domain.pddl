;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project domain problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain PROJECT)
  (:requirements :strips :typing :existential-preconditions :universal-preconditions)
  (:types
    box
    goal
    goal box - object
    agent
    cell
    letter
  )
  (:predicates
    (AgentAt ?a - agent ?c - cell)
    (Free ?c - cell)
    (Neighbour ?c1 - cell ?c2 - cell)
    (BoxAt ?b - box ?c - cell)
    (Letter ?o - object ?a - letter)
    (GoalAt ?g - goal ?c1 - cell)
  )

  (:action Move :parameters (?a - agent ?c1 - cell ?c2 - cell)
   :precondition (and (AgentAt ?a ?c1) (Free ?c2) (Neighbour ?c1 ?c2))
   :effect (and
    (not (AgentAt ?a ?c1)) (AgentAt ?a ?c2)
    (not (Free ?c2)) (Free ?c1) ))

  (:action Push :parameters (?a - agent ?b - box ?c1 - cell ?c2 - cell ?c3 - cell)
   :precondition (and
    (AgentAt ?a ?c1) (BoxAt ?b ?c2) (Free ?c3)
    (Neighbour ?c2 ?c3) (Neighbour ?c1 ?c2) )
   :effect (and
    (not (AgentAt ?a ?c1)) (AgentAt ?a ?c2)
    (not (BoxAt ?b ?c2)) (BoxAt ?b ?c3)
    (not (Free ?c3)) (Free ?c1) ))

  (:action Pull :parameters (?a - agent ?b - box ?c1 - cell ?c2 - cell ?c3 - cell)
   :precondition (and
    (AgentAt ?a ?c1) (BoxAt ?b ?c2) (Free ?c3)
    (Neighbour ?c1 ?c3) (Neighbour ?c1 ?c2) )
   :effect (and
    (not (AgentAt ?a ?c1)) (AgentAt ?a ?c3)
    (not (BoxAt ?b ?c2)) (BoxAt ?b ?c1)
    (not (Free ?c3)) (Free ?c2) ))
)
