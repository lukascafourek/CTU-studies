package cz.cvut.fel.pjv.model;

import cz.cvut.fel.pjv.services.BicycleVisitable;

public class RoadBike extends Bicycle {
    protected int tireWidth;
    public RoadBike(int cadence, int speed, int gear, int tireWidth) {
        super(cadence, speed, gear);
        this.tireWidth = tireWidth;
    }

    @Override
    public void printDescription() {
        super.printDescription();
        System.out.println("The RoadBike has " + this.tireWidth + "mm tires.");
    }
    public void visit(BicycleVisitable b) {
        b.accept(this);
    }
}
