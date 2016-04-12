/**
 * Created by miniwolf on 03-04-2016.
 */

import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;

import searchclient.SearchClient.Memory;
import searchclient.Node;

public abstract class Strategy {
    public HashSet<Node> explored;
    public long startTime = System.currentTimeMillis();
    private AdvancedHeuristic.AStar heuristic;

    public Strategy() {
        explored = new HashSet<>();
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

    public AdvancedHeuristic.AStar getHeuristic() {
        return heuristic;
    }

    // Ex 3: Best-first Search uses a priority queue (Java contains no implementation of a Heap data structure)
    public static class AdvancedStrategy extends Strategy {
        private PriorityQueue<Node> frontier;
        private Heuristic heuristic;

        public AdvancedStrategy(Heuristic h) {
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

    // Ex 3: Best-first Search uses a priority queue (Java contains no implementation of a Heap data structure)
    public static class PathStrategy extends Strategy {
        private PriorityQueue<Node> frontier;
        private PathHeuristic heuristic;

        public PathStrategy(PathHeuristic h) {
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

    /*public static class PathStrategy extends Strategy {
        private FibonacciHeap<Node> frontier;
        private PathHeuristic heuristic;
        private Map<Node, FibonacciHeap.Entry<Node>> entryMap = new HashMap<>();

        public PathStrategy(PathHeuristic h) {
            super();
            heuristic = h;
            frontier = new FibonacciHeap<>();
        }

        public Node getAndRemoveLeaf() {
            return frontier.dequeueMin().getValue(); // Head of the queue is the "least" element in the ordering using the comparator.
        }

        public void addToFrontier(Node n) {
            entryMap.put(n, frontier.enqueue(n, heuristic.f(n)));
        }

        public int countFrontier() {
            return frontier.size();
        }

        public boolean frontierIsEmpty() {
            return frontier.isEmpty();
        }

        public boolean inFrontier(Node n) {
            return frontier.contains(entryMap.get(n));
        }

        public String toString() {
            return "Path Search (PriorityQueue)";
        }
    }*/
}

