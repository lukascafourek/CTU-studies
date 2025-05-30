package cz.cvut.fel.pjv.model;

import cz.cvut.fel.pjv.services.BicycleVisitable;

public class Bicycle {
    protected int cadence;
    protected int speed;
    protected int gear;
    //protected final int type = 1;
    public Bicycle(int cadence, int speed, int gear) {
        this.cadence = cadence;
        this.speed = speed;
        this.gear = gear;
    }
    public void printDescription() {
        System.out.println("Bike is in gear " + this.gear + " with a cadence of " + this.cadence
                + " and travelling at a speed of " + this.speed + ".");
    }
    public void visit(BicycleVisitable b) {
        b.accept(this);
    }
/*    public String getDescription() {
        return ("Bike is in gear " + this.gear + " with a cadence of " + this.cadence
                + " and travelling at a speed of " + this.speed);
    }*/
}
