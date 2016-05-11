package core.singleagent;

import core.Agent;
import core.Position;

import java.util.HashSet;

/**
 * @author miniwolf
 */
public class SingleAgent extends Agent {
    private HashSet<Position> agentPath;

    public SingleAgent(Position position, int id) {
        super(position, id);
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

        SingleAgent agent = (SingleAgent) o;

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
