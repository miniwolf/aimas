package searchclient;

import java.util.*;

//import searchclient.SearchClient.Memory;

import searchclient.MultiSearchClient.Memory;
import searchclient.Node;

public abstract class Strategy {

    public HashSet<Node> explored;
    public long startTime = System.currentTimeMillis();
    private Heuristic.AStar heuristic;

    public Strategy() {
        explored = new HashSet<Node>();
    }

    public void addToExplored(Node n) {
        explored.add(n);
    }

    public boolean isExplored(Node n) {
        return explored.contains(n);
    }

    public int countExplored() {
        return explored.size();
    }

    public String searchStatus() {
        return String.format("#Explored: %4d, #Frontier: %3d, Time: %3.2f s \t%s", countExplored(), countFrontier(), timeSpent(), Memory.stringRep());
    }

    public float timeSpent() {
        return (System.currentTimeMillis() - startTime) / 1000f;
    }

    public abstract Node getAndRemoveLeaf();

    public abstract void addToFrontier(Node n);

    public abstract boolean inFrontier(Node n);

    public abstract int countFrontier();

    public abstract boolean frontierIsEmpty();

    public abstract String toString();

    public Heuristic.AStar getHeuristic() {
        return heuristic;
    }

    public static class StrategyBFS extends Strategy {
        private ArrayDeque<Node> frontier;

        public StrategyBFS() {
            super();
            frontier = new ArrayDeque<>();
        }

        public Node getAndRemoveLeaf() {
            return frontier.pollFirst();
        }

        public void addToFrontier(Node n) {
            frontier.addLast(n);
        }

        public int countFrontier() {
            return frontier.size();
        }

        public boolean frontierIsEmpty() {
            return frontier.isEmpty();
        }

        public boolean inFrontier(Node n) {
            return frontier.contains(n);
        }

        public String toString() {
            return "Breadth-first Search";
        }
    }

    public static class StrategyDFS extends Strategy {
        private Stack<Node> frontier;

        public StrategyDFS() {
            super();
            frontier = new Stack<>();
        }

        public Node getAndRemoveLeaf() {
            return frontier.pop();
        }

        public void addToFrontier(Node n) {
            frontier.push(n);
        }

        public int countFrontier() {
            return frontier.size();
        }

        public boolean frontierIsEmpty() {
            return frontier.isEmpty();
        }

        public boolean inFrontier(Node n) {
            return frontier.contains(n);
        }

        public String toString() {
            return "Depth-first Search";
        }
    }

    // Ex 3: Best-first Search uses a priority queue (Java contains no implementation of a Heap data structure)
    public static class StrategyBestFirst extends Strategy {
        private PriorityQueue<Node> frontier;
        private Heuristic heuristic;

        public StrategyBestFirst(Heuristic h) {
            super();
            heuristic = h;
            frontier = new PriorityQueue<>(1, (o1, o2) -> heuristic.compare(o1, o2));
        }

        public Node getAndRemoveLeaf() {
            return frontier.poll(); // Head of the queue is the "least" element in the ordering using the comparator.
        }

        public void addToFrontier(Node n) {
            frontier.add(n);
        }

        public int countFrontier() {
            return frontier.size();
        }

        public boolean frontierIsEmpty() {
            return frontier.isEmpty();
        }

        public boolean inFrontier(Node n) {
            return frontier.contains(n);
        }

        public String toString() {
            return "Best-first Search (PriorityQueue) using " + heuristic.toString();
        }
    }

    public static class StrategyIterativeDeepening extends Strategy {
        private Stack<Node> frontier;
        private Heuristic heuristic;

        public StrategyIterativeDeepening(Heuristic h) {
            super();
            heuristic = h;
            frontier = new Stack<>();
        }

        public Node getAndRemoveLeaf() {
            return frontier.pop();
        }

        public void addToFrontier(Node n) {
            frontier.push(n);
        }

        public int countFrontier() {
            return frontier.size();
        }

        public boolean frontierIsEmpty() {
            return frontier.isEmpty();
        }

        public boolean inFrontier(Node n) {
            return frontier.contains(n);
        }

        public String toString() {
            return "Best-first Search (PriorityQueue) using " + heuristic.toString();
        }
    }
}
