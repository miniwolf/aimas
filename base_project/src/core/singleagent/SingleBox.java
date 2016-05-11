package core.singleagent;

import core.Box;
import core.Position;

import java.util.ArrayList;
import java.util.List;

/**
 * @author miniwolf
 */
public class SingleBox extends Box {
    public SingleBox(Position position, Character character, int id) {
        super(position, character, id);
    }

    @Override
    public SingleBox clone() {
        SingleBox res = new SingleBox(position, character, id);
        res.setGoalPath(goalPath);
        res.setMovable(movable);
        res.setGoalLink(goalLink);
        return res;
    }

    @Override
    public boolean equals(Object o) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        SingleBox box = (SingleBox) o;

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
