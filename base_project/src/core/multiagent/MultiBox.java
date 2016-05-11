package core.multiagent;

import core.Box;
import core.Position;

import java.util.ArrayList;
import java.util.List;

/**
 * @author miniwolf
 */
public class MultiBox extends Box {
    private Position goalLink;
    private String color;
    private boolean movable = true;

    public MultiBox(Position position, Character character, int id, String color) {
        super(position, character, id);
        this.id = id;
        this.color = color;
        this.goalPath = new ArrayList<>();
    }

    public boolean isMovable() {
        return movable;
    }

    public void setMovable(boolean movable) {
        this.movable = movable;
    }

    public Position getGoalLink() {
        return goalLink;
    }

    public void setGoalLink(Position goalLink) {
        this.goalLink = goalLink;
    }

    @Override
    public MultiBox clone() {
        MultiBox res = new MultiBox(position, character, id, color);
        res.setGoalPath(goalPath);
        res.setMovable(movable);
        res.setGoalLink(goalLink);
        return res;
    }

    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        MultiBox box = (MultiBox) o;

        return id == box.id &&
               (position != null ? position.equals(box.position) : box.position == null);

    }

    @Override
    public int hashCode() {
        int result = position != null ? position.hashCode() : 0;
        result = 31 * result + id;
        return result;
    }
}
