package core;

import java.util.ArrayList;
import java.util.List;

/**
 * @author miniwolf
 */
public abstract class Box implements Cloneable {
    protected Position position;
    protected Character character;
    protected boolean movable = true;
    protected int id;
    protected List<Position> goalPath = new ArrayList<>();
    protected Position goalLink;

    public Box(Position position, Character character, int id) {
        this.position = position;
        this.character = character;
        this.id = id;
        this.goalPath = new ArrayList<>();
    }

    public Position getPosition() {
        return position;
    }

    public void setPosition(Position position) {
        this.position = position;
    }

    public Character getCharacter() {
        return character;
    }

    public boolean isMovable() {
        return movable;
    }

    public void setMovable(boolean movable) {
        this.movable = movable;
    }

    public int getId() {
        return id;
    }

    public void setGoalPath(List<Position> goalPath) {
        this.goalPath = goalPath;
    }

    public List<Position> getGoalPath() {
        return goalPath;
    }

    public Position getGoalLink() {
        return goalLink;
    }

    public void setGoalLink(Position goalLink) {
        this.goalLink = goalLink;
    }

    public abstract Box clone();
}
