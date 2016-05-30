package searchclient;

import searchclient.Command.dir;
import searchclient.Command.type;

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

    public static Set<Position> walls = new HashSet<>();
    public static Map<Position, Character> goals = new HashMap<>();
    public List<Box> boxes = new ArrayList<>();

    public Node parent;
    //public Command action;
    public List<Command> actions = new ArrayList<Command>(); // SJW

    private int g;
    public List<Agent> agents = new ArrayList<Agent>(); // SJW

    public Node(Node parent) {
        this.parent = parent;
        g = parent == null ? 0 : parent.g() + 1;
    }
    
    public void setAgents(List<Agent> agents) {
    	this.agents = agents;
    }
    
    public List<Agent> getAgents() {
    	return agents;
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
    
    // REF: http://www.programcreek.com/2013/02/leetcode-permutations-java/
    public ArrayList<ArrayList<Command>> permuteArray(List<Command> num) {
    	ArrayList<ArrayList<Command>> result = new ArrayList<ArrayList<Command>>();
     
    	//start from an empty list
    	result.add(new ArrayList<Command>());
     
    	for (int i = 0; i < num.size(); i++) {
    		//list of list in current iteration of the array num
    		ArrayList<ArrayList<Command>> current = new ArrayList<ArrayList<Command>>();
     
    		for (ArrayList<Command> l : result) {
    			// # of locations to insert is largest index + 1
    			for (int j = 0; j < l.size()+1; j++) {
    				// + add num[i] to different locations
    				//l.add(j, num[i]);
    				l.add(j, num.get(i));
     
    				ArrayList<Command> temp = new ArrayList<Command>(l);
    				current.add(temp);
     
    				//System.out.println(temp);
     
    				// - remove num[i] add
    				l.remove(j);
    			}
    		}
     
    		result = new ArrayList<ArrayList<Command>>(current);
    	}
    	System.out.println(result); // TRACE
    	return result;
    }
    
    /*
    public ArrayList<ArrayList<Command>> permute(Command[] num) {
    	ArrayList<ArrayList<Command>> result = new ArrayList<ArrayList<Command>>();
     
    	//start from an empty list
    	result.add(new ArrayList<Command>());
     
    	for (int i = 0; i < num.length; i++) {
    		//list of list in current iteration of the array num
    		ArrayList<ArrayList<Command>> current = new ArrayList<ArrayList<Command>>();
     
    		for (ArrayList<Command> l : result) {
    			// # of locations to insert is largest index + 1
    			for (int j = 0; j < l.size()+1; j++) {
    				// + add num[i] to different locations
    				l.add(j, num[i]);
     
    				ArrayList<Command> temp = new ArrayList<Command>(l);
    				current.add(temp);
     
    				//System.out.println(temp);
     
    				// - remove num[i] add
    				l.remove(j);
    			}
    		}
     
    		result = new ArrayList<ArrayList<Command>>(current);
    	}
    	//System.out.println(result); // TRACE
    	return result;
    }*/
    
    
    
    
    public ArrayList<Node> getExpandedNodes() {
        ArrayList<Node> expandedNodes = new ArrayList<>(Command.every.length);
         
        // Normal        
    	for(int i = 0; i < agents.size(); i++) {
    		for ( Command c : Command.every ) {
	            // Determine applicability of action
	    		int newAgentRow = agents.get(i).getPosition().getY() + dirToRowChange(c.dir1);
	    		int newAgentCol = agents.get(i).getPosition().getX() + dirToColChange(c.dir1);
	    		Position agentPos = new Position(newAgentCol, newAgentRow);
	    	        	
	            if ( c.actType == type.Move ) {
	                // Check if there's a wall or box on the cell to which the agent is moving
	                if ( !cellIsFree(newAgentCol, newAgentRow) ) {
	                    continue;
	                }
	                Node n = this.ChildNode();
	                
	                n.actions.add(c);
	                
	                
	                n.getAgents().get(i).setPosition(agentPos);
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
	                
	                n.actions.add(c);
	                
	                n.getAgents().get(i).setPosition(agentPos);
	                n.boxes.get(boxIdx).setPosition(new Position(newBoxCol, newBoxRow));
	                expandedNodes.add(n);
	            } else if ( c.actType == type.Pull ) {
	                // Cell is free where agent is going
	                if ( !cellIsFree(newAgentCol, newAgentRow) ) {
	                    continue;
	                }
	                int boxRow = agents.get(i).getPosition().getY() + dirToRowChange(c.dir2);
	                int boxCol = agents.get(i).getPosition().getX() + dirToColChange(c.dir2);
	                Position pos = new Position(boxCol, boxRow);
	
	                // .. and there's a box in "dir2" of the agent
	                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
	                if ( !b.isPresent() || !b.get().isMovable() ) {
	                    continue;
	                }
	                int boxIdx = boxes.indexOf(b.get());
	
	                Node n = this.ChildNode();
	                
	                n.actions.add(c);
	                
	                n.getAgents().get(i).setPosition(agentPos);
	                n.boxes.get(boxIdx).setPosition(agents.get(i).getPosition());

	                expandedNodes.add(n);
	            }
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
        
        // SJW
        List< Agent > ag = new ArrayList< Agent >();
        for(int i = 0; i < agents.size(); i++) {
        	ag.add(new Agent(agents.get(i).getPosition(), agents.get(i).getColor(), agents.get(i).getId()));
        }
        copy.setAgents(ag);
        return copy;
    }

    public LinkedList<Node> extractPlan() {
        LinkedList<Node> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
            n.boxes.forEach(box -> box.setMovable(true));
            plan.addFirst(n);
            n = n.parent;
        }
        return plan;
    }

    public LinkedList<Position> extractPath() {
        LinkedList<Position> plan = new LinkedList<>();
        Node n = this;
        while ( !n.isInitialState() ) {
        	for(int i = 0; i < agents.size(); i++) {
            	plan.addFirst(n.getAgents().get(i).getPosition());
        	}
            n = n.parent;
        }
        return plan;
    }

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
        Node other = (Node) obj;
        return agents.equals(other.agents) && boxes.equals(other.boxes);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        /*
        if ( action != null ) {
            s.append(action.toActionString()).append("\n");
        }*/
        for( int i = 0; i < actions.size(); i++ ) {
        	if( actions.get(i) != null) {
        		s.append(actions.get(i).toActionString()).append("\n");
        	}
        }

        
        for ( int row = 0; row < MAX_ROW; row++ ) {
            for ( int col = 0; col < MAX_COLUMN; col++ ) {
                Position pos = new Position(col, row);
                Optional<Box> b = boxes.stream().filter(box -> box.getPosition().equals(pos)).findFirst();
                if ( walls.contains(pos) ) {
                    s.append("+");
                } else if ( b.isPresent() ) {
                    s.append(b.get().getCharacter());
                } else if ( goals.containsKey(pos) ) {
                    s.append(goals.get(pos));
                    
                // SJW TEST FOR 2 AGENTS
                } else if ( agents.get(0).getPosition().equals(pos) ) {
                    s.append("0");
                } else if ( agents.get(1).getPosition().equals(pos) ) {
                	s.append("1");
                } else {
                    s.append(" ");
                }
            }

            s.append("\n");
        }
        return s.toString();
    }
}