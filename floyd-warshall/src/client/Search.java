package client;

import searchclient.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

/**
 * @author miniwolf
 */
public class Search {
    public static LinkedList<Node> search(Strategy.AdvancedStrategy strategy, Node initialState,
                                          int threshold, List<Position> dangerZone) throws IOException {
        System.err.format("Search starting with strategy %s\n", strategy);
        strategy.addToFrontier(initialState);
        boolean reachedThreshold = false;
        int iterations = 0;
        while ( true ) {
            if ( SearchClient.Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", SearchClient.Memory.stringRep());
                return null;
            }

            if ( strategy.frontierIsEmpty() ) {
                if ( reachedThreshold ) {
                    return new LinkedList<>();
                } else {
                    return null;
                }
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( leafNode.isGoalState(dangerZone) ) {
                System.err.println("\nSummary for " + strategy);
                System.err.println(strategy.searchStatus());
                System.err.println("\n");
                return leafNode.extractPlan();
            }

            if ( leafNode.g() > threshold ) {
                reachedThreshold = true;
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

    public static boolean agentGoalState
            (Node leafNode, Position goalBoxPos, int boxToRemoveId, HashSet<Position> boxPath,
             scala.collection.immutable.List<Position> immovableBoxes) {
        Box boxToRemove = leafNode.boxes.stream().filter(box -> box.getId() == boxToRemoveId).findFirst().get();
        Position boxToRemovePosition = boxToRemove.getPosition();
        Position agentPosition = leafNode.getAgent().getPosition();
        if ( Math.abs(boxToRemovePosition.getX() - agentPosition.getX()) + Math.abs(boxToRemovePosition.getY() - agentPosition.getY()) != 1 ) {
            return false;
        }
        scala.collection.immutable.List<Position> agentPath = PathFinding.searchEmpty(goalBoxPos, leafNode.getAgent().getPosition(), leafNode, immovableBoxes);
        if ( agentPath.isEmpty() ) {
            return false;
        }
        return !agentPath.contains(boxToRemovePosition) && !boxPath.contains(boxToRemovePosition);
    }

    public static LinkedList<Node> agentRemovalSearch
            (Strategy.AdvancedStrategy strategy, Node initialState, int threshold, int boxToRemoveId,
             List<Position> dangerZone, HashSet<Position> boxPath, Position goalBoxPos,
             scala.collection.immutable.List<Position> immovableBoxes, int depth) throws IOException {
        System.err.format("client.Search starting with strategy %s\n", strategy);
        strategy.addToFrontier(initialState);

        int iterations = 0;
        List<Node> solutionLists = new ArrayList<>();
        while ( true ) {
            if ( SearchClient.Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", SearchClient.Memory.stringRep());
                return null;
            }

            if ( strategy.frontierIsEmpty() ) {
                if ( !solutionLists.isEmpty() ) {
                    return solutionLists.get(solutionLists.size() - 1).extractPlan();
                }
                return null;
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( agentGoalState(leafNode, goalBoxPos, boxToRemoveId, boxPath, immovableBoxes) ) {
                if ( solutionLists.size() < depth ) {
                    solutionLists.add(leafNode);
                } else {
                    System.err.println("\nSummary for " + strategy);
                    System.err.println(strategy.searchStatus());
                    System.err.println("\n");
                    return leafNode.extractPlan();
                }
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
}
