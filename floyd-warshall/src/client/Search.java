package client;

import searchclient.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

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

    private static boolean extendedGoalState(Node leafNode, Position goalBoxPos, int boxToRemoveId,
                                             scala.collection.immutable.HashSet<Position> boxPath,
                                             scala.collection.immutable.List<Position> immovableBoxes,
                                             scala.collection.immutable.List<Position> needToAvoid,
                                             scala.collection.immutable.List<Position> dangerZone) {
        Box boxToRemove = leafNode.boxes.stream().filter(box -> box.getId() == boxToRemoveId).findFirst().get();
        Position boxToRemovePosition = boxToRemove.getPosition();
        if ( needToAvoid.contains(boxToRemovePosition) ) {
            return false;
        }
        List<Box> boxesMovable = leafNode.boxes.stream().filter(Box::isMovable).collect(Collectors.toList());
        List<Box> issues = boxesMovable.stream().filter(box -> dangerZone.contains(box.getPosition())).collect(Collectors.toList());
        if ( !issues.isEmpty() ) {
            return false;
        }

        //val dangerZones = findDangerousPositions(vertices, edges, tempBoxGoal, realGoalBox, box.getPosition, lockedNode.ChildNode())
        // TODO: use danger zone to stop the agent from blocking itself from the actual goal box.
        Position agentPosition = leafNode.getAgent().getPosition();
        if ( Math.abs(boxToRemovePosition.getX() - agentPosition.getX()) + Math.abs(boxToRemovePosition.getY() - agentPosition.getY()) != 1 ) {
            return false;
        }
        scala.collection.immutable.List<Position> agentPath = PathFinding.searchEmpty(goalBoxPos, leafNode.getAgent().getPosition(), leafNode, immovableBoxes);
        return agentPath.nonEmpty() && !agentPath.contains(boxToRemovePosition) && !boxPath.contains(boxToRemovePosition);
    }

    public static LinkedList<Node> removalSearch
            (Strategy.AdvancedStrategy strategy, Node initialState, int threshold, int boxToRemoveId,
             scala.collection.immutable.HashSet<Position> boxPath, Position goalBoxPos,
             scala.collection.immutable.List<Position> immovableBoxes,
             scala.collection.immutable.List<Position> needToAvoid,
             scala.collection.immutable.List<Position> dangerZone) throws IOException {
        System.err.format("client.Search starting with strategy %s\n", strategy);
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

            if ( extendedGoalState(leafNode, goalBoxPos, boxToRemoveId, boxPath, immovableBoxes, needToAvoid, dangerZone) ) {
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
}
