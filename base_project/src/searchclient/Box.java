package searchclient;

/**
 * Created by miniwolf on 31-03-2016.
 */
public class Box {
    private Position position;
    private Character character;

    public Box(Position position, Character character) {
        this.position = position;
        this.character = character;
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
}
