package cz.cvut.fel.pjv.services;

import cz.cvut.fel.pjv.model.Bicycle;
import cz.cvut.fel.pjv.model.MountainBike;
import cz.cvut.fel.pjv.model.RoadBike;

public class BasicService implements BicycleVisitable {
    public void accept(Bicycle b) {
//        if (b instanceof MountainBike || b instanceof RoadBike) {
//            System.out.println("can`t fix");
//        } else {
//            System.out.println("fixing Bicycle");
//        }
        System.out.println("fixing Bicycle");
    }
    public void accept(MountainBike mb) {
        System.out.println("can`t fix MountainBike");
    }
    public void accept(RoadBike rb) {
        System.out.println("can`t fix RoadBike");
    }
}
