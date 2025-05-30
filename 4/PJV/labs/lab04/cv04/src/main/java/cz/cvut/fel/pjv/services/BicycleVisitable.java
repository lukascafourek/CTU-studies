package cz.cvut.fel.pjv.services;

import cz.cvut.fel.pjv.model.Bicycle;
import cz.cvut.fel.pjv.model.MountainBike;
import cz.cvut.fel.pjv.model.RoadBike;

public interface BicycleVisitable {
    public void accept(Bicycle bike);
    public void accept(MountainBike bike);
    public void accept(RoadBike bike);

    //TODO dopln dalsi viz Ukol 5
}
