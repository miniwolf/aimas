package searchclient;

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
        List<Box> boxes = n.boxes;
        Map<searchclient.Position, Character> goals = Node.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 1; i < Node.MAX_ROW; i++ ) {
            for ( int j = 1; j < Node.MAX_COLUMN; j++ ) {
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
                double bestDistance = Node.MAX_COLUMN + Node.MAX_ROW;
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

    public int h2(Node n) {
        List<Box> boxes = n.boxes;
        Map<Position, Character> goals = Node.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 0; i < Node.MAX_ROW; i++ ) {
            for ( int j = 0; j < Node.MAX_COLUMN; j++ ) {
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
            	            	
            	for(int i = 0; i < n.getAgents().size(); i++) {
                    value += 2 * Math.sqrt((bestPosition.getX() - n.getAgents().get(i).getPosition().getX()) *
                    (bestPosition.getX() - n.getAgents().get(i).getPosition().getX()) +
                    (bestPosition.getY() - n.getAgents().get(i).getPosition().getY()) *
                    (bestPosition.getY() - n.getAgents().get(i).getPosition().getY()));
            	}
                /*value += 2 * Math.sqrt((bestPosition.getX() - n.getAgent().getPosition().getX()) *
                                       (bestPosition.getX() - n.getAgent().getPosition().getX()) +
                                       (bestPosition.getY() - n.getAgent().getPosition().getY()) *
                                       (bestPosition.getY() - n.getAgent().getPosition().getY()));
                */
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

    public static class WeightedAStar extends Heuristic {
        private int W;

        public WeightedAStar(Node initialState) {
            super(initialState);
            W = 5; // You're welcome to test this out with different values, but for the reporting part you must at least indicate benchmarks for W = 5
        }

        public int f(Node n) {
            return n.g() + W * h(n);
        }

        public String toString() {
            return String.format("WA*(%d) evaluation", W);
        }
    }

    public static class Greedy extends Heuristic {

        public Greedy(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return h(n);
        }

        public String toString() {
            return "Greedy evaluation";
        }
    }

    public static class IDAStar extends Heuristic {
        public IDAStar(Node initialState) {
            super(initialState);
        }

        @Override
        public int f(Node n) {
            return n.g() + h(n);
        }

        @Override
        public String toString() {
            return "IDA* evaluation";
        }
    }

    public static class AStar2 extends Heuristic {
        public AStar2(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return n.g() + h2(n);
        }

        public String toString() {
            return "A* evaluation";
        }
    }

    public static class WeightedAStar2 extends Heuristic {
        private int W;

        public WeightedAStar2(Node initialState) {
            super(initialState);
            W = 5; // You're welcome to test this out with different values, but for the reporting part you must at least indicate benchmarks for W = 5
        }

        public int f(Node n) {
            return n.g() + W * h2(n);
        }

        public String toString() {
            return String.format("WA*(%d) evaluation", W);
        }
    }

    public static class Greedy2 extends Heuristic {

        public Greedy2(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return h2(n);
        }

        public String toString() {
            return "Greedy evaluation";
        }
    }

    public static class IDAStar2 extends Heuristic {
        public IDAStar2(Node initialState) {
            super(initialState);
        }

        @Override
        public int f(Node n) {
            return n.g() + h2(n);
        }

        @Override
        public String toString() {
            return "IDA* evaluation";
        }
    }
}
