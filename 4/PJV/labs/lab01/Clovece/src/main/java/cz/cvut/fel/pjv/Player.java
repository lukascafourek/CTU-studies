package cz.cvut.fel.pjv;

public class Player {

    private final String name;

    protected int currentPosition;

    Dice dice;

    public Player(String name, Dice dice) {
        this.name = name;
        this.dice = dice;
        currentPosition = 0;
    }

    public int advance() {
        int thrown = dice.throwIt();
        if (thrown != 6 && currentPosition == 0) {
            return currentPosition;
        }
        if (thrown != 6) {
            currentPosition += thrown;
            return currentPosition;
        }
        int t;
        do {
            t = dice.throwIt();
            thrown += t;
        } while (t == 6);
        currentPosition += thrown;
        return currentPosition;
    }

    public String getName() {
        return name;
    }

    public int getCurrentPosition() {
        return currentPosition;
    }
}
