import javafx.geometry.Pos;
import searchclient.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * @author miniwolf
 */
public class Search {
    public static LinkedList<Node> search(Strategy.AdvancedStrategy strategy, Node initialState, int threshold) throws IOException {
        System.err.format("Search starting with strategy %s\n", strategy);
        strategy.addToFrontier(initialState);

        int iterations = 0;
        while ( true ) {
            if ( SearchClient.Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", SearchClient.Memory.stringRep());
                return null;
            }

            if ( strategy.frontierIsEmpty() ) {
                return null;
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( leafNode.isGoalState() ) {
                System.err.println("\nSummary for " + strategy);
                System.err.println(strategy.searchStatus());
                System.err.println("\n");
                return leafNode.extractPlan();
            }

            if ( leafNode.g() > threshold ) {
                continue;
            }

            if ( iterations % 200 == 0 ) {
                System.err.println(strategy.searchStatus());
            }

            strategy.addToExplored(leafNode);
            ArrayList<Node> expandedNodes = leafNode.getExpandedNodes();
            // The list of expanded nodes is shuffled randomly; see Node.java
            expandedNodes.stream().filter(n -> !strategy.isExplored(n) && !strategy.inFrontier(n)).forEach(strategy::addToFrontier);
            iterations++;
        }
    }

    public static List<Position> search(Strategy.PathStrategy strategy, Node initialState, Position goalState) {
        strategy.addToFrontier(initialState);

        while ( true ) {
            if ( strategy.frontierIsEmpty() ) {
                return null;
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( leafNode.getAgent().getPosition().equals(goalState) ) {
                return new ArrayList<>(leafNode.extractPath());
            }

            strategy.addToExplored(leafNode);
            ArrayList<Node> expandedNodes = leafNode.getExpandedNodes();
            // The list of expanded nodes is shuffled randomly; see Node.java
            expandedNodes.stream().filter(n -> !strategy.isExplored(n) && !strategy.inFrontier(n)).forEach(strategy::addToFrontier);
        }
    }
}
