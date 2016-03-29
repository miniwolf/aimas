package searchclient;

import java.awt.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public abstract class Heuristic implements Comparator<Node> {

    public Node initialState;

    public Heuristic(Node initialState) {
        this.initialState = initialState;
    }

    public int compare(Node n1, Node n2) {
        return f(n1) - f(n2);
    }

    private class Position {
        int x, y;
        public Position(int x, int y) {
            this.x = x; this.y = y;
        }
    }

    public int h(Node n) {
        char[][] boxes = n.boxes;
        char[][] goals = Node.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 0; i < Node.MAX_ROW; i++ ) {
            for ( int j = 0; j < Node.MAX_COLUMN; j++ ) {
                char box = boxes[i][j];
                char goal = goals[i][j];
                if ( Character.isLetter(goal) && Character.isLetter(box) &&
                        Character.toUpperCase(goal) == box ) {
                    continue;
                }
                if ( Character.isLetter(goal) ) {
                    goalsChars.put(new Position(i, j), goal);
                }
                if ( Character.isLetter(box) ) {
                    boxesChars.put(new Position(i, j), box);
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
                    double newDistance = Math.sqrt((newPos.x - pos.x) * (newPos.x - pos.x) + (newPos.y - pos.y) * (newPos.y - pos.y));
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
        char[][] boxes = n.boxes;
        char[][] goals = Node.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 0; i < Node.MAX_ROW; i++ ) {
            for ( int j = 0; j < Node.MAX_COLUMN; j++ ) {
                char box = boxes[i][j];
                char goal = goals[i][j];
                if ( Character.isLetter(goal) && Character.isLetter(box) &&
                        Character.toUpperCase(goal) == box ) {
                    continue;
                }
                if ( Character.isLetter(goal) ) {
                    goalsChars.put(new Position(i, j), goal);
                }
                if ( Character.isLetter(box) ) {
                    boxesChars.put(new Position(i, j), box);
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
                double newDistance = Math.sqrt((newPos.x - pos.x) * (newPos.x - pos.x) + (newPos.y - pos.y) * (newPos.y - pos.y));
                if ( newDistance < bestDistance ) {
                    bestDistance = newDistance;
                    bestPosition = newPos;
                }
            }

            value += bestDistance;
            if ( bestPosition != null ) {
                value += 2 * Math.sqrt((bestPosition.x - n.agentRow) * (bestPosition.x - n.agentRow) + (bestPosition.y - n.agentCol) * (bestPosition.y - n.agentCol));
            }
        }
        return (int) value;
    }
	
	// Manhattan distance
	public int h3(Node n) {
		char[][] boxes = n.boxes;
        char[][] goals = Node.goals;
        HashMap<Position, Character> goalsChars = new HashMap<>();
        HashMap<Position, Character> boxesChars = new HashMap<>();
        for ( int i = 0; i < Node.MAX_ROW; i++ ) {
            for ( int j = 0; j < Node.MAX_COLUMN; j++ ) {
                char box = boxes[i][j];
                char goal = goals[i][j];
                if ( Character.isLetter(goal) && Character.isLetter(box) &&
                        Character.toUpperCase(goal) == box ) {
                    continue;
                }
                if ( Character.isLetter(goal) ) {
                    goalsChars.put(new Position(i, j), goal);
                }
                if ( Character.isLetter(box) ) {
                    boxesChars.put(new Position(i, j), box);
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
					double newDistance = Math.abs(newPos.x - pos.x) + Math.abs(newPos.y - pos.y);
                    if ( newDistance < bestDistance ) {
                        bestDistance = newDistance;
                    }
                }
                value += bestDistance;
            }
        }
        return value;
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
	
	public static class AStar3 extends Heuristic {
        public AStar3(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return n.g() + h3(n);
        }

        public String toString() {
            return "A3* evaluation";
        }
    }
	
	public static class WeightedAStar3 extends Heuristic {
        private int W;

        public WeightedAStar3(Node initialState) {
            super(initialState);
            W = 5; // You're welcome to test this out with different values, but for the reporting part you must at least indicate benchmarks for W = 5
        }

        public int f(Node n) {
            return n.g() + W * h3(n);
        }

        public String toString() {
            return String.format("WA3*(%d) evaluation", W);
        }
    }

    public static class Greedy3 extends Heuristic {

        public Greedy3(Node initialState) {
            super(initialState);
        }

        public int f(Node n) {
            return h3(n);
        }

        public String toString() {
            return "Greedy3 evaluation";
        }
    }

    public static class IDAStar3 extends Heuristic {
        public IDAStar3(Node initialState) {
            super(initialState);
        }

        @Override
        public int f(Node n) {
            return n.g() + h3(n);
        }

        @Override
        public String toString() {
            return "IDA3* evaluation";
        }
    }

}
