package core.multiagent;

import core.Agent;
import core.Position;

import java.util.HashSet;

/**
 * @author miniwolf
 */
public class MultiAgent extends Agent {
    private String color;
    private HashSet<Position> agentPath;

    public MultiAgent(Position position, int id, String color) {
        super(position, id);
        this.color = color;
    }


    public void setAgentPath(HashSet<Position> agentPath) {
        this.agentPath = agentPath;
    }

    public HashSet<Position> getAgentPath() {
        return agentPath;
    }

    public String getColor() {
        return color;
    }

    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        MultiAgent agent = (MultiAgent) o;

        return id == agent.id && color.equals(agent.color) &&
               (position != null ? position.equals(agent.position) : agent.position == null);
    }

    @Override
    public int hashCode() {
        int result = position != null ? position.hashCode() : 0;
        result = 31 * result + id;
        return result;
    }
}

