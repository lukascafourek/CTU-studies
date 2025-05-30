package cz.cvut.fel.pjv;

import java.util.Random;

public class Dice {

    private final int sides;

    public Dice(int sides) {
        this.sides = sides;
    }

    public int throwIt() {
        Random rnd = new Random();
        return rnd.nextInt(sides) + 1;
    }
}
