
(define (problem projprob1) (:domain PROJECT)
   (:requirements :typing)
   (:objects
box0 box1 box2 box3 - box
letterc letterb letterAcap lettera letterd letterBcap letterDcap letterCcap - letter
goal0 goal1 goal2 goal3 - goal
cell1x1 cell1x3 cell1x4 cell1x5 cell1x6 cell2x1 cell2x2 cell2x4 cell2x5 cell2x6 cell3x1 cell3x3 cell3x5 cell3x6 cell4x1 cell4x2 cell4x3 cell4x4 cell4x6 cell5x1 cell5x6 cell6x1 cell6x2 cell6x3 cell6x4 cell6x5 cell6x6 - cell
agent0 - agent
)
   (:init
(Neighbour cell1x1 cell2x1)
(AgentAt agent0 cell1x1)
(Neighbour cell1x3 cell1x4)
(Free cell1x3)
(Neighbour cell1x4 cell2x4)
(Neighbour cell1x4 cell1x3)
(Neighbour cell1x4 cell1x5)
(Free cell1x4)
(Neighbour cell1x5 cell2x5)
(Neighbour cell1x5 cell1x4)
(Neighbour cell1x5 cell1x6)
(GoalAt goal0 cell1x5)
(Letter goal0 letterc)
(Free cell1x5)
(Neighbour cell1x6 cell2x6)
(Neighbour cell1x6 cell1x5)
(GoalAt goal1 cell1x6)
(Letter goal1 letterb)
(Free cell1x6)
(Neighbour cell2x1 cell1x1)
(Neighbour cell2x1 cell3x1)
(Neighbour cell2x1 cell2x2)
(Free cell2x1)
(Neighbour cell2x2 cell2x1)
(BoxAt box0 cell2x2)
(Letter box0 letterAcap)
(Neighbour cell2x4 cell1x4)
(Neighbour cell2x4 cell2x5)
(Free cell2x4)
(Neighbour cell2x5 cell1x5)
(Neighbour cell2x5 cell3x5)
(Neighbour cell2x5 cell2x4)
(Neighbour cell2x5 cell2x6)
(GoalAt goal2 cell2x5)
(Letter goal2 lettera)
(Free cell2x5)
(Neighbour cell2x6 cell1x6)
(Neighbour cell2x6 cell3x6)
(Neighbour cell2x6 cell2x5)
(GoalAt goal3 cell2x6)
(Letter goal3 letterd)
(Free cell2x6)
(Neighbour cell3x1 cell2x1)
(Neighbour cell3x1 cell4x1)
(Free cell3x1)
(Neighbour cell3x3 cell4x3)
(BoxAt box1 cell3x3)
(Letter box1 letterBcap)
(Neighbour cell3x5 cell2x5)
(Neighbour cell3x5 cell3x6)
(Free cell3x5)
(Neighbour cell3x6 cell2x6)
(Neighbour cell3x6 cell4x6)
(Neighbour cell3x6 cell3x5)
(Free cell3x6)
(Neighbour cell4x1 cell3x1)
(Neighbour cell4x1 cell5x1)
(Neighbour cell4x1 cell4x2)
(Free cell4x1)
(Neighbour cell4x2 cell4x1)
(Neighbour cell4x2 cell4x3)
(Free cell4x2)
(Neighbour cell4x3 cell3x3)
(Neighbour cell4x3 cell4x2)
(Neighbour cell4x3 cell4x4)
(BoxAt box2 cell4x3)
(Letter box2 letterDcap)
(Neighbour cell4x4 cell4x3)
(BoxAt box3 cell4x4)
(Letter box3 letterCcap)
(Neighbour cell4x6 cell3x6)
(Neighbour cell4x6 cell5x6)
(Free cell4x6)
(Neighbour cell5x1 cell4x1)
(Neighbour cell5x1 cell6x1)
(Free cell5x1)
(Neighbour cell5x6 cell4x6)
(Neighbour cell5x6 cell6x6)
(Free cell5x6)
(Neighbour cell6x1 cell5x1)
(Neighbour cell6x1 cell6x2)
(Free cell6x1)
(Neighbour cell6x2 cell6x1)
(Neighbour cell6x2 cell6x3)
(Free cell6x2)
(Neighbour cell6x3 cell6x2)
(Neighbour cell6x3 cell6x4)
(Free cell6x3)
(Neighbour cell6x4 cell6x3)
(Neighbour cell6x4 cell6x5)
(Free cell6x4)
(Neighbour cell6x5 cell6x4)
(Neighbour cell6x5 cell6x6)
(Free cell6x5)
(Neighbour cell6x6 cell5x6)
(Neighbour cell6x6 cell6x5)
(Free cell6x6))
   (:goal (and (BoxAt box3 cell1x5)
               (BoxAt box0 cell2x5)
               (BoxAt box1 cell1x6)
               (BoxAt box2 cell2x6))
)
)
