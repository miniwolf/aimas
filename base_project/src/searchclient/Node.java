package searchclient;

import searchclient.Command.dir;
import searchclient.Command.type;

import java.util.*;

public class Node {
    private static Random rnd = new Random(1);
    public static int MAX_ROW = 70;
    public static int MAX_COLUMN = 70;

    public int agentRow;
    public int agentCol;

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

    public Node(Node parent) {
        this.parent = parent;
        g = parent == null ? 0 : parent.g() + 1;
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
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                Character boxChar;
                if ( b.isPresent() ) {
                    boxChar = b.get().getCharacter();
                } else {
                    return false;
                }
                if ( g == null ) {
                    continue;
                }
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
            int newAgentRow = this.agentRow + dirToRowChange(c.dir1);
            int newAgentCol = this.agentCol + dirToColChange(c.dir1);

            if ( c.actType == type.Move ) {
                // Check if there's a wall or box on the cell to which the agent is moving
                if ( cellIsFree(newAgentCol, newAgentRow) ) {
                    Node n = this.ChildNode();
                    n.action = c;
                    n.agentRow = newAgentRow;
                    n.agentCol = newAgentCol;
                    expandedNodes.add(n);
                }
            } else if ( c.actType == type.Push ) {
                // Make sure that there's actually a box to move
                if ( boxAt(newAgentCol, newAgentRow) ) {
                    int newBoxRow = newAgentRow + dirToRowChange(c.dir2);
                    int newBoxCol = newAgentCol + dirToColChange(c.dir2);
                    // .. and that new cell of box is free
                    if ( cellIsFree(newBoxCol, newBoxRow) ) {
                        Node n = this.ChildNode();
                        n.action = c;
                        n.agentRow = newAgentRow;
                        n.agentCol = newAgentCol;
                        Position pos = new Position(newAgentCol, newAgentRow);
                        Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                        if ( b.isPresent() ) {
                            b.get().setPosition(new Position(newBoxCol, newBoxRow));
                        }
                        expandedNodes.add(n);
                    }
                }
            } else if ( c.actType == type.Pull ) {
                // Cell is free where agent is going
                if ( cellIsFree(newAgentCol, newAgentRow) ) {
                    int boxRow = this.agentRow + dirToRowChange(c.dir2);
                    int boxCol = this.agentCol + dirToColChange(c.dir2);
                    // .. and there's a box in "dir2" of the agent
                    if ( boxAt(boxCol, boxRow) ) {
                        Node n = this.ChildNode();
                        n.action = c;
                        n.agentRow = newAgentRow;
                        n.agentCol = newAgentCol;
                        Position pos = new Position(boxCol, boxRow);
                        Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                        if ( b.isPresent() ) {
                            b.get().setPosition(new Position(agentCol, agentRow));
                        }
                        expandedNodes.add(n);
                    }
                }
            }
        }
        Collections.shuffle(expandedNodes, rnd);
        return expandedNodes;
    }

    private boolean cellIsFree(int col, int row) {
        Position pos = new Position(col, row);
        return !Node.walls.contains(pos) && !(this.boxes.stream().filter(box -> box.getPosition().equals(pos)).count() == 1);
    }

    private boolean boxAt(int col, int row) {
        Position pos = new Position(col, row);
        return this.boxes.stream().filter(box -> box.getPosition().equals(pos)).count() == 1;
    }

    private int dirToRowChange(dir d) {
        return d == dir.S ? 1 : d == dir.N ? -1 : 0; // South is down one row (1), north is up one row (-1)
    }

    private int dirToColChange(dir d) {
        return d == dir.E ? 1 : d == dir.W ? -1 : 0; // East is left one column (1), west is right one column (-1)
    }

    public Node ChildNode() {
        Node copy = new Node(this);
        //for ( int row = 0; row < MAX_ROW; row++ ) {
            //System.arraycopy(this.walls[row], 0, copy.walls[row], 0, MAX_COLUMN);
        copy.boxes = (ArrayList<Box>) ((ArrayList<Box>) this.boxes).clone();
            //System.arraycopy(this.goals[row], 0, copy.goals[row], 0, MAX_COLUMN);
        //}
        return copy;
    }

    public LinkedList<Node> extractPlan() {
        LinkedList<Node> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            plan.addFirst(n);
            n = n.parent;
        }
        return plan;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + agentCol;
        result = prime * result + agentRow;
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
        return agentCol == other.agentCol && agentRow == other.agentRow && boxes.equals(other.boxes);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        for ( int row = 0; row < MAX_ROW; row++ ) {
            if ( !walls.contains(new Position(0, row)) ) {
                break;
            }
            for ( int col = 0; col < MAX_COLUMN; col++ ) {
                Position pos = new Position(col, row);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                if ( b.isPresent() ) {
                    s.append(b.get().getCharacter());
                } else if ( goals.containsKey(pos) ) {
                    s.append(goals.get(pos));
                } else if ( walls.contains(pos) ) {
                    s.append("+");
                } else if ( row == this.agentRow && col == this.agentCol ) {
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