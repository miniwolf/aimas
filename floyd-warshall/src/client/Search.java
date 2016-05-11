package client;

import core.Box;
import core.Node;
import core.Position;
import searchclient.SearchClient;
import searchclient.SearchResult;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author miniwolf
 */
public class Search {
    public static SearchResult search(Strategy.AdvancedStrategy strategy, Node initialState,
                                      int threshold, HashSet<Position> dangerZone, List<Box> notNeededBoxes) throws IOException {
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
                return new SearchResult(null, reachedThreshold);
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( leafNode.isGoalState(dangerZone, notNeededBoxes) ) {
                System.err.println("\nSummary for " + strategy);
                System.err.println(strategy.searchStatus());
                System.err.println("\n");
                return new SearchResult(leafNode.extractPlan(), reachedThreshold);
            }

            if ( leafNode.g() > threshold ) {
                reachedThreshold = true;
                continue;
            }

            if ( iterations % 200 == 0 ) {
                System.err.println(strategy.searchStatus());
            }

            strategy.addToExplored(leafNode);
            List<Node> expandedNodes = leafNode.getExpandedNodes();
            // The list of expanded nodes is shuffled randomly; see Node.java
            expandedNodes.stream().filter(n -> !strategy.isExplored(n) && !strategy.inFrontier(n)).forEach(strategy::addToFrontier);
            iterations++;
        }
    }

    private static boolean extendedGoalState(Node leafNode, int goalBoxId, int boxToRemoveId,
                                             scala.collection.immutable.HashSet<Position> boxPath,
                                             scala.collection.immutable.List<Position> immovableBoxes,
                                             scala.collection.immutable.List<Position> needToAvoid,
                                             scala.collection.immutable.HashSet<Position> dangerZone) {
        Box boxToRemove = leafNode.getBoxes().stream().filter(box -> box.getId() == boxToRemoveId).findFirst().get();
        Position boxToRemovePosition = boxToRemove.getPosition();
        Position agentPosition = leafNode.getAgent().getPosition();
        if ( Math.abs(boxToRemovePosition.getX() - agentPosition.getX()) + Math.abs(boxToRemovePosition.getY() - agentPosition.getY()) != 1 ) {
            return false;
        }
        if ( needToAvoid.contains(boxToRemovePosition) ) {
            return false;
        }
        List<Box> boxesMovable = leafNode.getBoxes().stream().filter(Box::isMovable).collect(Collectors.toList());
        List<Box> issues = boxesMovable.stream().filter(box -> dangerZone.contains(box.getPosition())).collect(Collectors.toList());
        if ( !issues.isEmpty() ) {
            return false;
        }

        scala.collection.immutable.List<Position> agentPath = PathFinding.searchEmpty(goalBoxId, leafNode.getAgent().getPosition(), leafNode, immovableBoxes);
        return agentPath.nonEmpty() && !agentPath.contains(boxToRemovePosition) && !boxPath.contains(boxToRemovePosition);
    }

    public static SearchResult removalSearch
            (Strategy.AdvancedStrategy strategy, Node initialState, int threshold, int boxToRemoveId,
             scala.collection.immutable.HashSet<Position> boxPath, int goalBoxId,
             scala.collection.immutable.List<Position> immovableBoxes,
             scala.collection.immutable.List<Position> needToAvoid,
             scala.collection.immutable.HashSet<Position> dangerZone) throws IOException {
        System.err.format("client.Search starting with strategy %s\n", strategy);
        strategy.addToFrontier(initialState);

        int iterations = 0;
        boolean reachedThreshold = false;
        while ( true ) {
            if ( SearchClient.Memory.shouldEnd() ) {
                System.err.format("Memory limit almost reached, terminating search %s\n", SearchClient.Memory.stringRep());
                return null;
            }

            if ( strategy.frontierIsEmpty() ) {
                return new SearchResult(null, reachedThreshold);
            }

            Node leafNode = strategy.getAndRemoveLeaf();

            if ( extendedGoalState(leafNode, goalBoxId, boxToRemoveId, boxPath, immovableBoxes, needToAvoid, dangerZone) ) {
                System.err.println("\nSummary for " + strategy);
                System.err.println(strategy.searchStatus());
                System.err.println("\n");
                return new SearchResult(leafNode.extractPlan(), false);
            }

            if ( leafNode.g() > threshold ) {
                reachedThreshold = true;
                continue;
            }

            if ( iterations % 200 == 0 ) {
                System.err.println(strategy.searchStatus());
            }

            strategy.addToExplored(leafNode);
            // The list of expanded nodes is shuffled randomly; see Node.java
            leafNode.getExpandedNodes().stream().filter(n -> !strategy.isExplored(n) && !strategy.inFrontier(n)).forEach(strategy::addToFrontier);
            iterations++;
        }
    }
}
