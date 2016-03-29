package searchclient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

import searchclient.Heuristic.*;
import searchclient.Strategy.*;

public class SearchClient {
    public Node initialState = null;

    public SearchClient(BufferedReader serverMessages) throws Exception {
        Map<Character, String> colors = new HashMap<>();
        String line, color;

        int agentCol = -1, agentRow = -1;
        int colorLines = 0, levelLines = 0;

        // Read lines specifying colors
        while ( (line = serverMessages.readLine()).matches("^[a-z]+:\\s*[0-9A-Z](,\\s*[0-9A-Z])*\\s*$") ) {
            line = line.replaceAll("\\s", "");
            String[] colonSplit = line.split(":");
            color = colonSplit[0].trim();

            for ( String id : colonSplit[1].split(",") ) {
                colors.put(id.trim().charAt(0), color);
            }
            colorLines++;
        }

        if ( colorLines > 0 ) {
            error("Box colors not supported");
        }

        initialState = new Node(null);

        int lineLength = 0;
        while ( line != null && !line.equals("") ) {
            if ( line.length() > lineLength ) {
                lineLength = line.length();
            }
            for ( int i = 0; i < line.length(); i++ ) {
                char chr = line.charAt(i);
                if ( '+' == chr ) { // Walls
                    Node.walls[levelLines][i] = true;
                } else if ( '0' <= chr && chr <= '9' ) { // Agents
                    if ( agentCol != -1 || agentRow != -1 ) {
                        error("Not a single agent level");
                    }
                    initialState.agentRow = levelLines;
                    initialState.agentCol = i;
                } else if ( 'A' <= chr && chr <= 'Z' ) { // Boxes
                    initialState.boxes[levelLines][i] = chr;
                } else if ( 'a' <= chr && chr <= 'z' ) { // Goal cells
                    Node.goals[levelLines][i] = chr;
                }
            }
            line = serverMessages.readLine();
            levelLines++;
        }
        Node.MAX_COLUMN = lineLength;
        Node.MAX_ROW = levelLines;
    }

    // Auxiliary static classes
    public static void error(String msg) throws Exception {
        throw new Exception("GSCError: " + msg);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader serverMessages = new BufferedReader(new InputStreamReader(System.in));

        // Use stderr to print to console
        System.err.println("SearchClient initializing. I am sending this using the error output stream.");

        // Read level and create the initial state of the problem
        SearchClient client = new SearchClient(serverMessages);

        Strategy strategy;
        //strategy = new StrategyBFS();
        // Ex 1:
        //strategy = new StrategyDFS();

        // Ex 3:
        //strategy = new StrategyBestFirst(new AStar(client.initialState));
        //strategy = new StrategyBestFirst(new WeightedAStar(client.initialState));
        //strategy = new StrategyBestFirst(new Greedy(client.initialState));
        //strategy = new StrategyBestFirst(new IDAStar(client.initialState));

        //strategy = new StrategyBestFirst(new AStar2(client.initialState));
        //strategy = new StrategyBestFirst(new WeightedAStar2(client.initialState));
        //strategy = new StrategyBestFirst(new Greedy2(client.initialState));
        //strategy = new StrategyBestFirst(new IDAStar2(client.initialState));
		
		//strategy = new StrategyBestFirst(new AStar3(client.initialState));
        strategy = new StrategyBestFirst(new WeightedAStar3(client.initialState));
        //strategy = new StrategyBestFirst(new Greedy3(client.initialState));
        //strategy = new StrategyBestFirst(new IDAStar3(client.initialState));

        LinkedList<Node> solution = client.Search(strategy);
        //LinkedList<Node> solution = client.searchIterative(strategy);

        if ( solution == null ) {
            System.err.println("Unable to solve level");
            System.exit(0);
        } else {
            System.err.println("\nSummary for " + strategy);
            System.err.println("Found solution of length " + solution.size());
            System.err.println(strategy.searchStatus());

            for ( Node n : solution ) {
                String act = n.action.toActionString();
                System.out.println(act);
                String response = serverMessages.readLine();
                if ( response.contains("false") ) {
                    System.err.format("Server responsed with %s to the inapplicable action: %s\n", response, act);
                    System.err.format("%s was attempted in \n%s\n", act, n);
                    break;
                }
            }
        }
    }

    public LinkedList<Node> Search(Strategy strategy) throws IOException {
        System.err.format("Search starting with strategy %s\n", strategy);
        strategy.addToFrontier(this.initialState);

        int iterations = 0;
        while ( true ) {
            if ( iterations % 200 == 0 ) {
                System.err.println(strategy.searchStatus());
            }
            if ( Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", Memory.stringRep());
                return null;
            }
            if ( strategy.timeSpent() > 300 ) { // Minutes timeout
                System.err.format("Time limit reached, terminating search %s\n", Memory.stringRep());
                return null;
            }

            if ( strategy.frontierIsEmpty() ) {
                return null;
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( leafNode.isGoalState() ) {
                return leafNode.extractPlan();
            }

            strategy.addToExplored(leafNode);
            ArrayList<Node> expandedNodes = leafNode.getExpandedNodes();
            for ( Node n : expandedNodes ) { // The list of expanded nodes is shuffled randomly; see Node.java
                if ( !strategy.isExplored(n) && !strategy.inFrontier(n) ) {
                    strategy.addToFrontier(n);
                }
            }
            iterations++;
        }
    }

    public LinkedList<Node> searchIterative(Strategy strategy) throws IOException {
        System.err.format("Search starting with strategy %s\n", strategy);

        int iterations = 0;
        Node root = this.initialState;
        AStar aStar = new AStar(root);
        int bound = aStar.f(root);
        while ( true ) {
            if ( iterations % 200 == 0 ) {
                System.err.println(strategy.searchStatus());
            }
            if ( Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", Memory.stringRep());
                return null;
            }
            if ( strategy.timeSpent() > 300 ) { // Minutes timeout
                System.err.format("Time limit reached, terminating search %s\n", Memory.stringRep());
                return null;
            }

            Node t = search(aStar, root, 0, bound);
            if ( t.isGoalState() ) {
                return t.extractPlan();
            }
            bound = aStar.f(t);
            iterations++;
        }
    }

    private int cost(Node node, Node succ) {
        return node.g() - succ.g();
    }

    public Node search(Heuristic.AStar astar, Node node, int g, int bound) {
        int f = g + astar.h(node);
        if ( f > bound || node.isGoalState() ) {
            return node;
        }

        Node min = null;
        //strategy.addToExplored(leafNode);
        for ( Node succ : node.getExpandedNodes() ) { // The list of expanded nodes is shuffled randomly; see Node.java
            //strategy.addToFrontier(succ);
            Node t = search(astar, succ, g + cost(node, succ), bound);
            if ( t != null && t.isGoalState() ) {
                return t;
            }
            if ( min == null || g + cost(node, succ) + astar.h(t) < g + cost(node, min) + astar.h(min) ) {
                min = t;
            }
        }
        return min;
    }


    public static class Memory {
        public static final float mb = 1024 * 1024;
        public static final float limitRatio = .9f;
        public static final int timeLimit = 180;
        public static Runtime runtime = Runtime.getRuntime();

        public static float used() {
            return (runtime.totalMemory() - runtime.freeMemory()) / mb;
        }

        public static float free() {
            return runtime.freeMemory() / mb;
        }

        public static float total() {
            return runtime.totalMemory() / mb;
        }

        public static float max() {
            return runtime.maxMemory() / mb;
        }

        public static boolean shouldEnd() {
            return (used() / max() > limitRatio);
        }

        public static String stringRep() {
            return String.format("[Used: %.2f MB, Free: %.2f MB, Alloc: %.2f MB, MaxAlloc: %.2f MB]", used(), free(), total(), max());
        }
    }
}
