/*package searchclient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

import searchclient.Heuristic.*;
import searchclient.Strategy.*;

public class SearchClient {
    public Node initialState = null;

    public SearchClient(List<String> lines) throws Exception {
        Map<Character, String> colors = new HashMap<>();
        String line, color;

        int colorLines = 0, levelLines = 0;

        // Read lines specifying colors
        int index = 0;
        while ( (line = lines.get(index)).matches("^[a-z]+:\\s*[0-9A-Z](,\\s*[0-9A-Z])*\\s*$") ) {
            line = line.replaceAll("\\s", "");
            String[] colonSplit = line.split(":");
            color = colonSplit[0].trim();

            for ( String id : colonSplit[1].split(",") ) {
                colors.put(id.trim().charAt(0), color);
            }
            colorLines++;
            index++;
        }

        if ( colorLines > 0 ) {
            error("Box colors not supported");
        }

        initialState = new Node(null);
        int id = 0;
        for ( int i = index; i < lines.size(); i++ ) {
            line = lines.get(i);
            for ( int j = 0; j < line.length(); j++ ) {
                char chr = line.charAt(j);
                Position pos = new Position(j, levelLines);
                if ( '+' == chr ) { // Walls
                    Node.walls.add(pos);
                } else if ( '0' <= chr && chr <= '9' ) { // Agents
                    initialState.setAgent(new Agent(pos, colors.get( id ), 0)); // TODO: Does not support MA
                } else if ( 'A' <= chr && chr <= 'Z' ) { // Boxes
                    initialState.boxes.add(new Box(pos, chr, id));
                    id++;
                } else if ( 'a' <= chr && chr <= 'z' ) { // Goal cells
                    Node.goals.put(pos, chr);
                }
            }

            levelLines++;
        }
    }

    // Auxiliary static classes
    public static void error(String msg) throws Exception {
        throw new Exception("GSCError: " + msg);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader serverMessages = new BufferedReader(new InputStreamReader(System.in));

        // Use stderr to print to console
        System.err.println("SearchClient initializing. I am sending this using the error output stream.");
        List<String> lines = new ArrayList<>();
        String line;
        while ( !(line = serverMessages.readLine()).equals("") ) {
            lines.add(line);
        }
        Node.MAX_COLUMN = lines.stream().mapToInt(String::length).max().getAsInt();
        Node.MAX_ROW = lines.size();


        // Read level and create the initial state of the problem
        SearchClient client = new SearchClient(lines);

        Strategy strategy;
        //strategy = new StrategyBFS();
        // Ex 1:
        //strategy = new StrategyDFS();

        // Ex 3:
        strategy = new StrategyBestFirst( new AStar2( client.initialState ) );
        //strategy = new StrategyBestFirst( new WeightedAStar2( client.initialState ) );
        //strategy = new StrategyBestFirst( new Greedy2( client.initialState ) );

        LinkedList<Node> solution = client.Search(strategy);

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
            // The list of expanded nodes is shuffled randomly; see Node.java
            expandedNodes.stream().filter(n -> !strategy.isExplored(n) && !strategy.inFrontier(n)).forEach(strategy::addToFrontier);
            iterations++;
        }
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
*/