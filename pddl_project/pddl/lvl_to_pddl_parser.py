import sys


def get_text_from_file(filepath):
    with open(filepath, 'r') as f:
        return f.read()


PDDL_PROBLEM_TEMPLATE = """
(define (problem {problem_name}) (:domain {domain_name})
   {requirements}
   (:objects \n{objects})
   (:init \n{init})
   (:goal \n{goal})
)
"""

problem_name = "projprob1"
domain_name = "PROJECT"
requirements = "(:requirements :typing)"
initials = []
objects = {
    "agent": [],
    "cell": [],
    "box": [],
    "goal": [],
    "letter": []
}

CELL_TEMPLATE = "cell%dx%d"
AGENT_TEMPLATE = "agent%d"
BOX_TEMPLATE = "box%d"
GOAL_TEMPLATE = "goal%d"
LETTER_TEMPLATE = "letter%s"

NEIGHBOUR_TEMPLATE = "(Neighbour %s %s)"  # % (CELL_TEMPLATE, CELL_TEMPLATE)
AGENT_AT_TEMPLATE = "(AgentAt %s %s)"  # % (AGENT_TEMPLATE, CELL_TEMPLATE)
FREE_TEMPLATE = "(Free %s)"  # % CELL_TEMPLATE
BOX_AT_TEMPLATE = "(BoxAt %s %s)"  # % (BOX_TEMPLATE, CELL_TEMPLATE)
GOAL_AT_TEMPLATE = "(GoalAt %s %s)"  # % (GOAL_TEMPLATE, CELL_TEMPLATE)
LETTER_ON_TEMPLATE = "(Letter %s %s)"  # % (BOX_TEMPLATE, LETTER_TEMPLATE)

goal_id = 0
box_id = 0
agent_id = 0

level = get_text_from_file(sys.argv[1]).split('\n')

for i, line in enumerate(level):
    for j, char in enumerate(line):
        cell = CELL_TEMPLATE % (i, j)
        if char != '+':
            if i - 1 >= 0 and level[i - 1][j] != '+':
                cell2 = CELL_TEMPLATE % (i - 1, j)
                initials.append(NEIGHBOUR_TEMPLATE % (cell, cell2))
            if i + 1 < len(level) and level[i + 1][j] != '+':
                cell2 = CELL_TEMPLATE % (i + 1, j)
                initials.append(NEIGHBOUR_TEMPLATE % (cell, cell2))
            if j - 1 >= 0 and level[i][j - 1] != '+':
                cell2 = CELL_TEMPLATE % (i, j - 1)
                initials.append(NEIGHBOUR_TEMPLATE % (cell, cell2))
            if j + 1 < len(level[i]) and level[i][j + 1] != '+':
                cell2 = CELL_TEMPLATE % (i, j + 1)
                initials.append(NEIGHBOUR_TEMPLATE % (cell, cell2))
        else:
            continue
        objects["cell"].append(cell)
        if char == ' ':
            initials.append(FREE_TEMPLATE % cell)
        elif char.isdigit():
            agent = AGENT_TEMPLATE % agent_id
            objects["agent"].append(agent)
            initials.append(AGENT_AT_TEMPLATE % (agent, cell))
            agent_id += 1
        elif char.isalpha():
            letter = LETTER_TEMPLATE % char
            if char.islower():
                goal = GOAL_TEMPLATE % goal_id
                objects["goal"].append(goal)
                initials.append(GOAL_AT_TEMPLATE % (goal, cell))
                initials.append(LETTER_ON_TEMPLATE % (goal, letter))
                initials.append(FREE_TEMPLATE % cell)
                goal_id += 1
            elif char.isupper():
                letter += "cap"
                box = BOX_TEMPLATE % box_id
                objects["box"].append(box)
                initials.append(BOX_AT_TEMPLATE % (box, cell))
                initials.append(LETTER_ON_TEMPLATE % (box, letter))
                box_id += 1
            if letter not in objects["letter"]:
                objects["letter"].append(letter)

initials_string = "\n".join(initials)
object_string = ""
for key in objects.keys():
    object_string += "{} - {}\n".format(" ".join(objects[key]), key)
text = PDDL_PROBLEM_TEMPLATE.format(problem_name=problem_name, domain_name=domain_name, requirements=requirements,
                                    objects=object_string, init=initials_string, goal="")

if len(sys.argv) == 3:
    with open(sys.argv[2], "w") as f:
        f.write(text)
else:
    print(text)
