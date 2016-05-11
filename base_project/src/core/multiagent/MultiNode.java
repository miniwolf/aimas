package core.multiagent;

import core.Command.dir;
import core.*;

import java.util.*;

public class MultiNode extends Node {
    private static Random rnd = new Random(1);
    public static int MAX_ROW = 70;
    public static int MAX_COLUMN = 70;

    public List<Box> boxes = new ArrayList<>();

    public Command action;

    private int g;
    private Map<Integer, MultiAgent> agents;
    private Agent currentAgent;

    public MultiNode(Node parent) {
        super(parent);
        g = parent == null ? 0 : parent.g() + 1;
    }

    public static void clear() {
        walls.clear();
        goals.clear();
    }

    public void putAgent(MultiAgent agent) {
        agents.put(agent.getId(), agent);
    }

    public Map<Integer, MultiAgent> getAgents() {
        return agents;
    }

    @Override
    public List<Box> getBoxes() {
        return boxes;
    }

    public int g() {
        return g;
    }

    @Override
    public Agent getAgent() {
        return currentAgent;
    }

    @Override
    public boolean isGoalState(HashSet<Position> dangerZone, List<Box> notNeededBoxes) {
        for ( MultiAgent agent : agents.values() ) {
            if ( dangerZone.contains(agent.getPosition()) ||
                 boxes.stream().anyMatch(box -> !notNeededBoxes.contains(box) && dangerZone.contains(box.getPosition())) ) {
                return false;
            }
        }
        return super.isGoalState();
    }

    public List<Node> getExpandedNodes() {
        List<Node> expandedNodes = new ArrayList<>(Command.every.length);
        for ( Command c : Command.every ) {
            // Determine applicability of action
            int newAgentRow = currentAgent.getPosition().getY() + dirToRowChange(c.dir1);
            int newAgentCol = currentAgent.getPosition().getX() + dirToColChange(c.dir1);
            Position agentPos = new Position(newAgentCol, newAgentRow);

            switch (c.actType) {
                case Move: {
                    // Check if there's a wall or box on the cell to which the agent is moving
                    if ( !cellIsFree(newAgentCol, newAgentRow) ) {
                        continue;
                    }
                    MultiNode n = this.ChildNode();
                    n.action = c;
                    n.getAgents().get(currentAgent.getId()).setPosition(agentPos);
                    expandedNodes.add(n);
                    break;
                }
                case Pull: {
                    // Cell is free where agent is going
                    if ( !cellIsFree(newAgentCol, newAgentRow) ) {
                        continue;
                    }
                    int boxRow = currentAgent.getPosition().getY() + dirToRowChange(c.dir2);
                    int boxCol = currentAgent.getPosition().getX() + dirToColChange(c.dir2);
                    Position pos = new Position(boxCol, boxRow);

                    // .. and there's a box in "dir2" of the agent
                    Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                    if ( !b.isPresent() || !b.get().isMovable() ) {
                        continue;
                    }
                    int boxIdx = boxes.indexOf(b.get());

                    MultiNode n = this.ChildNode();
                    n.action = c;
                    n.getAgents().get(currentAgent.getId()).setPosition(agentPos);
                    n.boxes.get(boxIdx).setPosition(currentAgent.getPosition());

                    expandedNodes.add(n);
                    break;
                }
                case Push: {
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
                    MultiNode n = this.ChildNode();
                    n.action = c;
                    n.getAgents().get(currentAgent.getId()).setPosition(agentPos);
                    n.boxes.get(boxIdx).setPosition(new Position(newBoxCol, newBoxRow));
                    expandedNodes.add(n);
                    break;
                }

            }
        }
        Collections.shuffle(expandedNodes, rnd);
        return expandedNodes;
    }

    @Override
    public String getAction() {
        return null;
    }

    private boolean cellIsFree(int col, int row) {
        Position pos = new Position(col, row);
        boolean boxAt = this.boxes.stream().anyMatch(box -> box.getPosition().equals(pos));
        boolean agentAt = this.agents.values().stream().anyMatch(multiAgent -> multiAgent.getPosition().equals(pos));
        return !MultiNode.walls.contains(pos) && !boxAt && !agentAt;
    }

    private int dirToRowChange(dir d) {
        return d == dir.S ? 1 : d == dir.N ? -1 : 0; // South is down one row (1), north is up one row (-1)
    }

    private int dirToColChange(dir d) {
        return d == dir.E ? 1 : d == dir.W ? -1 : 0; // East is left one column (1), west is right one column (-1)
    }

    public MultiNode ChildNode() {
        MultiNode copy = new MultiNode(this);
        copy.boxes = new ArrayList<>();
        boxes.forEach(box -> copy.boxes.add(box.clone()));
        agents.values().forEach(agent -> copy.putAgent(new MultiAgent(agent.getPosition(), agent.getId(), agent.getColor())));
        return copy;
    }

    public LinkedList<Node> extractPlan() {
        LinkedList<Node> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            n.getBoxes().forEach(box -> box.setMovable(true));
            plan.addFirst(n);
            n = n.parent;
        }
        return plan;
    }

    /*public LinkedList<Position> extractPath() {
        LinkedList<Position> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            plan.addFirst(n.getAgent().getPosition());
            n = n.parent;
        }
        return plan;
    }*/

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + agents.hashCode();
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
        MultiNode other = (MultiNode) obj;
        if ( boxes.size() != other.boxes.size() ) {
            return false;
        }
        for ( Box box : boxes ) {
            if ( !other.boxes.contains(box) ) {
                return false;
            }
        }
        for ( Integer agentID : agents.keySet() ) {
            if ( !other.agents.containsKey(agentID) ) {
                return false;
            }
        }
        return true;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        if ( action != null ) {
            s.append(action.toActionString()).append("\n");
        }
        for ( int row = 0; row < MAX_ROW; row++ ) {
            for ( int col = 0; col < MAX_COLUMN; col++ ) {
                Position pos = new Position(col, row);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                Optional<MultiAgent> a = agents.values().stream().filter(agent -> agent.getPosition().equals(pos)).findFirst();
                if ( walls.contains(pos) ) {
                    s.append("+");
                } else if ( b.isPresent() ) {
                    Box box = b.get();
                    if ( box.isMovable() ) {
                        s.append(box.getCharacter());
                    } else {
                        s.append("-");
                    }
                } else if ( goals.containsKey(pos) ) {
                    s.append(goals.get(pos));
                } else if ( a.isPresent() ) {
                    s.append(a.get().getId());
                } else {
                    s.append(" ");
                }
            }

            s.append("\n");
        }
        return s.toString();
    }
}