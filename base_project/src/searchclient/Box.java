package searchclient;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by miniwolf on 31-03-2016.
 */
public class Box implements Cloneable {
    private Position position;
    private Character character;
    private List<Position> goalPath;
    private int id;
    private boolean movable = true;

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

    public List<Position> getGoalPath() {
        return goalPath;
    }

    public void setGoalPath(List<Position> goalPath) {
        this.goalPath = goalPath;
    }

    public int getId() {
        return id;
    }

    public boolean isMovable() {
        return movable;
    }

    public void setMovable(boolean movable) {
        this.movable = movable;
    }

    @Override
    public Box clone() {
        Box res = new Box(position, character, id);
        res.setGoalPath(goalPath);
        res.setMovable(movable);
        return res;
    }

    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Box box = (Box) o;

        if ( id != box.id ) return false;
        return position != null ? position.equals(box.position) : box.position == null;

    }

    @Override
    public int hashCode() {
        int result = position != null ? position.hashCode() : 0;
        result = 31 * result + id;
        return result;
    }
}
