package core;

import core.singleagent.SingleNode;

import java.util.*;

/**
 * @author miniwolf
 */
public abstract class Node {
    public static int MAX_ROW = 70;
    public static int MAX_COLUMN = 70;

    private int g;

    public static Set<Position> walls = new HashSet<>();
    public static Map<Position, Character> goals = new HashMap<>();
    public Node parent;

    public Node(Node parent) {
        this.parent = parent;
        g = parent == null ? 0 : parent.g() + 1;
    }

    public boolean isInitialState() {
        return this.parent == null;
    }

    public abstract List<Box> getBoxes();

    public int g() {
        return g;
    }

    public abstract Node ChildNode();

    public abstract List<Node> extractPlan();

    public abstract Agent getAgent();

    public abstract List<Node> getExpandedNodes();

    public abstract String getAction();

    public abstract boolean isGoalState(HashSet<Position> dangerZone, List<Box> notNeededBoxes);

    protected boolean isGoalState() {
        for ( Position pos: SingleNode.goals.keySet() ) {
            Character g = goals.get(pos);

            Optional<Box> b = getBoxes().stream().filter(box -> box.getPosition().equals(pos)).findFirst();
            if ( !b.isPresent() ) {
                return false;
            }
            Character boxChar = b.get().getCharacter();
            if ( g > 0 && Character.toLowerCase(boxChar) != g ) {
                return false;
            }
        }
        return true;
    }
}
