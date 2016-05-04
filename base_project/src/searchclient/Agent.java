package searchclient;
import java.util.Random;

/**
 * @author miniwolf
 */


public class Agent {
	private static Random rand = new Random(); // SJW

    private Position position;
    private String color; // SJW
    private int id;

    public Agent(Position position, String color, int id) {
        this.position = position;
        this.color = color;
        this.id = id;
    }

    public Position getPosition() {
        return position;
    }

    public void setPosition(Position position) {
        this.position = position;
    }

    public int getId() {
        return id;
    }
    
    // SJW
    public String getColor() {
    	return color;
    }
    
    // SJW
    public void setColor() {
    	this.color = color;
    }
    
    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Agent agent = (Agent) o;

        return id == agent.id && color == agent.color &&
               (position != null ? position.equals(agent.position) : agent.position == null);
    }

    @Override
    public int hashCode() {
        int result = position != null ? position.hashCode() : 0;
        result = 31 * result + id;
        return result;
    }
}
