package searchclient;

import searchclient.Command.dir;
import searchclient.Command.type;

import java.nio.file.Path;
import java.util.*;

public class Node {
    private static Random rnd = new Random(1);
    public static int MAX_ROW = 70;
    public static int MAX_COLUMN = 70;

    // Arrays are indexed from the top-left of the level, with first index being row and second being column.
    // Row 0: (0,0) (0,1) (0,2) (0,3) ...
    // Row 1: (1,0) (1,1) (1,2) (1,3) ...
    // Row 2: (2,0) (2,1) (2,2) (2,3) ...
    //

    public static List<Position> walls = new ArrayList<>();
    public static Map<Position, Character> goals = new HashMap<>();
    public List<Box> boxes = new ArrayList<>();

    public Node parent;
    public Command action;

    private int g;
    private Agent agent;

    public Node(Node parent) {
        this.parent = parent;
        g = parent == null ? 0 : parent.g() + 1;
    }

    public void setAgent(Agent agent) {
        this.agent = agent;
    }

    public Agent getAgent() {
        return agent;
    }

    public int g() {
        return g;
    }

    public boolean isInitialState() {
        return this.parent == null;
    }

    public boolean isGoalState() {
        for ( int row = 1; row < MAX_ROW - 1; row++ ) {
            for ( int col = 1; col < MAX_COLUMN - 1; col++ ) {
                Position pos = new Position(col, row);
                Character g = goals.get(pos);
                if ( g == null ) {
                    continue;
                }

                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                if ( !b.isPresent() ) {
                    return false;
                }
                Character boxChar = b.get().getCharacter();
                if ( g > 0 && Character.toLowerCase(boxChar) != g ) {
                    return false;
                }
            }
        }
        return true;
    }

    public ArrayList<Node> getExpandedNodes() {
        ArrayList<Node> expandedNodes = new ArrayList<>(Command.every.length);
        for ( Command c : Command.every ) {
            // Determine applicability of action
            int newAgentRow = agent.getPosition().getY() + dirToRowChange(c.dir1);
            int newAgentCol = agent.getPosition().getX() + dirToColChange(c.dir1);
            Position agentPos = new Position(newAgentCol, newAgentRow);

            if ( c.actType == type.Move ) {
                // Check if there's a wall or box on the cell to which the agent is moving
                if ( !cellIsFree(newAgentCol, newAgentRow) ) {
                    continue;
                }
                Node n = this.ChildNode();
                n.action = c;
                n.getAgent().setPosition(agentPos);
                expandedNodes.add(n);
            } else if ( c.actType == type.Push ) {
                // Make sure that there's actually a box to move
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(agentPos)).findFirst();
                if ( !b.isPresent() || !b.get().isMovable() ) {
                    continue;
                }
                int boxIdx = boxes.indexOf(b.get());
                int newBoxRow = newAgentRow + dirToRowChange(c.dir2);
                int newBoxCol = newAgentCol + dirToColChange(c.dir2);
                // .. and that new cell of box is free
                if ( !cellIsFree(newBoxCol, newBoxRow) ) {
                    continue;
                }
                Node n = this.ChildNode();
                n.action = c;
                n.getAgent().setPosition(agentPos);
                n.boxes.get(boxIdx).setPosition(new Position(newBoxCol, newBoxRow));
                expandedNodes.add(n);
            } else if ( c.actType == type.Pull ) {
                // Cell is free where agent is going
                if ( !cellIsFree(newAgentCol, newAgentRow) ) {
                    continue;
                }
                int boxRow = agent.getPosition().getY() + dirToRowChange(c.dir2);
                int boxCol = agent.getPosition().getX() + dirToColChange(c.dir2);
                Position pos = new Position(boxCol, boxRow);

                // .. and there's a box in "dir2" of the agent
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                if ( !b.isPresent() || !b.get().isMovable() ) {
                    continue;
                }
                int boxIdx = boxes.indexOf(b.get());

                Node n = this.ChildNode();
                n.action = c;
                n.getAgent().setPosition(agentPos);
                n.boxes.get(boxIdx).setPosition(agent.getPosition());

                expandedNodes.add(n);
            }
        }
        Collections.shuffle(expandedNodes, rnd);
        return expandedNodes;
    }

    private boolean cellIsFree(int col, int row) {
        Position pos = new Position(col, row);
        boolean boxAt = this.boxes.stream().filter(box -> box.getPosition().equals(pos)).count() == 1;
        return !Node.walls.contains(pos) && !boxAt;
    }

    private int dirToRowChange(dir d) {
        return d == dir.S ? 1 : d == dir.N ? -1 : 0; // South is down one row (1), north is up one row (-1)
    }

    private int dirToColChange(dir d) {
        return d == dir.E ? 1 : d == dir.W ? -1 : 0; // East is left one column (1), west is right one column (-1)
    }

    public Node ChildNode() {
        Node copy = new Node(this);
        copy.boxes = new ArrayList<>();
        boxes.forEach(box -> copy.boxes.add(box.clone()));
        copy.setAgent(new Agent(agent.getPosition(), agent.getId()));
        return copy;
    }

    public LinkedList<Node> extractPlan() {
        LinkedList<Node> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            plan.addFirst(n);
            n.boxes.forEach(box -> box.setMovable(true));
            n = n.parent;
        }
        return plan;
    }

    public LinkedList<Position> extractPath() {
        LinkedList<Position> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            plan.addFirst(n.getAgent().getPosition());
            n = n.parent;
        }
        return plan;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + agent.hashCode();
        result = prime * result + boxes.hashCode();
        result = prime * result + goals.hashCode();
        result = prime * result + walls.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if ( this == obj )
            return true;
        if ( obj == null )
            return false;
        if ( getClass() != obj.getClass() )
            return false;
        Node other = (Node) obj;
        return agent.equals(other.agent) && boxes.equals(other.boxes);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        if ( action != null ) {
            s.append(action.toActionString()).append("\n");
        }
        for ( int row = 0; row < MAX_ROW; row++ ) {
            /*if ( !walls.contains(new Position(0, row)) ) {
                break;
            }*/
            for ( int col = 0; col < MAX_COLUMN; col++ ) {
                Position pos = new Position(col, row);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                if ( b.isPresent() ) {
                    s.append(b.get().getCharacter());
                } else if ( goals.containsKey(pos) ) {
                    s.append(goals.get(pos));
                } else if ( walls.contains(pos) ) {
                    s.append("+");
                } else if ( agent.getPosition().equals(pos) ) {
                    s.append("0");
                } else {
                    s.append(" ");
                }
            }

            s.append("\n");
        }
        return s.toString();
    }
}