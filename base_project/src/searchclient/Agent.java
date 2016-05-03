package searchclient;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * @author miniwolf
 */
public class Agent {
    private Position position;
    private int id;
    private HashSet<Position> agentPath;

    public Agent(Position position, int id) {
        this.position = position;
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

    public void setAgentPath(HashSet<Position> agentPath) {
        this.agentPath = agentPath;
    }

    public HashSet<Position> getAgentPath() {
        return agentPath;
    }

    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Agent agent = (Agent) o;

        return id == agent.id &&
               (position != null ? position.equals(agent.position) : agent.position == null);
    }

    @Override
    public int hashCode() {
        int result = position != null ? position.hashCode() : 0;
        result = 31 * result + id;
        return result;
    }
}
