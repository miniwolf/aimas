package core;

import core.singleagent.SingleBox;
import core.singleagent.SingleNode;

import java.util.*;
import java.util.stream.Collectors;

public abstract class Heuristic implements Comparator<Node> {
    public Node initialState;

    public Heuristic(Node initialState) {
        this.initialState = initialState;
    }

    public int compare(Node n1, Node n2) {
        return f(n1) - f(n2);
    }

    public int h(Node n) {
        List<Box> boxes = n.getBoxes();
        Map<Position, Character> goals = SingleNode.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 1; i < SingleNode.MAX_ROW; i++ ) {
            for ( int j = 1; j < SingleNode.MAX_COLUMN; j++ ) {
                Position pos = new Position(i, j);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                Character boxChar = b.isPresent() ? b.get().getCharacter() : null;
                Character goal = goals.get(pos);
                if ( boxChar != null && goal != null ) {
                    if ( Character.isLetter(goal) && Character.isLetter(boxChar) &&
                         Character.toUpperCase(goal) == boxChar ) {
                        continue;
                    }
                }
                if ( boxChar != null && Character.isLetter(boxChar) ) {
                    boxesChars.put(pos, boxChar);
                }
                if ( goal != null && Character.isLetter(goal) ) {
                    goalsChars.put(pos, goal);
                }
            }
        }
        int value = 0;
        for ( Position pos : boxesChars.keySet() ) {
            char c = boxesChars.get(pos);
            char lowerC = Character.toLowerCase(c);
            List<Position> positions = goalsChars.keySet().stream().filter(index -> goalsChars.get(index) == lowerC)
                    .collect(Collectors.toList());
            if ( !positions.isEmpty() ) {
                double bestDistance = SingleNode.MAX_COLUMN + SingleNode.MAX_ROW;
                for ( Position newPos : positions ) {
                    double newDistance = Math.sqrt((newPos.getX() - pos.getX()) * (newPos.getX() - pos.getX()) + (newPos.getY() - pos.getY()) * (newPos.getY() - pos.getY()));
                    if ( newDistance < bestDistance ) {
                        bestDistance = newDistance;
                    }
                }
                value += bestDistance;
            }
        }
        return value;
    }

    public int h2(SingleNode n) {
        List<Box> boxes = n.getBoxes();
        Map<Position, Character> goals = SingleNode.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 0; i < SingleNode.MAX_ROW; i++ ) {
            for ( int j = 0; j < SingleNode.MAX_COLUMN; j++ ) {
                Position pos = new Position(i, j);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                Character boxChar = b.isPresent() ? b.get().getCharacter() : null;
                Character goal = goals.get(pos);
                if ( boxChar != null && goal != null ) {
                    if ( Character.isLetter(goal) && Character.isLetter(boxChar) &&
                         Character.toUpperCase(goal) == boxChar ) {
                        continue;
                    }
                }
                if ( boxChar != null && Character.isLetter(boxChar) ) {
                    boxesChars.put(pos, boxChar);
                }
                if ( goal != null && Character.isLetter(goal) ) {
                    goalsChars.put(pos, goal);
                }

            }
        }
        double value = 0;
        for ( Position pos : goalsChars.keySet() ) {
            char goal = goalsChars.get(pos);
            char upperGoal = Character.toUpperCase(goal);
            List<Position> positions = boxesChars.keySet().stream().filter(index -> boxesChars.get(index) == upperGoal)
                    .collect(Collectors.toList());
            assert !positions.isEmpty();
            double bestDistance = Integer.MAX_VALUE;
            Position bestPosition = null;
            for ( Position newPos : positions ) {
                double newDistance = Math.sqrt((newPos.getX() - pos.getX()) * (newPos.getX() - pos.getX()) + (newPos.getY() - pos.getY()) * (newPos.getY() - pos.getY()));
                if ( newDistance < bestDistance ) {
                    bestDistance = newDistance;
                    bestPosition = newPos;
                }
            }

            value += bestDistance;
            if ( bestPosition != null ) {
                value += 2 * Math.sqrt((bestPosition.getX() - n.getAgent().getPosition().getX()) *
                                       (bestPosition.getX() - n.getAgent().getPosition().getX()) +
                                       (bestPosition.getY() - n.getAgent().getPosition().getY()) *
                                       (bestPosition.getY() - n.getAgent().getPosition().getY()));
            }
        }
        return (int) value;
    }

    public abstract int f(Node n);

    public static class AStar extends Heuristic {
        public AStar(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return n.g() + h(n);
        }

        public String toString() {
            return "A* evaluation";
        }
    }
}
