package cz.cvut.fel.pjv.model;

import cz.cvut.fel.pjv.services.BicycleVisitable;

public class MountainBike extends Bicycle {
    protected String suspension;
    public MountainBike(int cadence, int speed, int gear, String suspension) {
        super(cadence, speed, gear);
        this.suspension = suspension;
    }

    @Override
    public void printDescription() {
        super.printDescription();
        System.out.println("The MountainBike has " + this.suspension + " suspension.");
    }
    public void visit(BicycleVisitable b) {
        b.accept(this);
    }
}

