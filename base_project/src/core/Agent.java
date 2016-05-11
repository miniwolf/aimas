package core;

import java.util.HashSet;

/**
 * @author miniwolf
 */
public class Agent {
    protected Position position;
    protected int id;

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
}
