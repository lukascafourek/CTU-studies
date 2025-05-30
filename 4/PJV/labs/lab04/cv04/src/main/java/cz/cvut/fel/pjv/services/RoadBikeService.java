package cz.cvut.fel.pjv.services;

import cz.cvut.fel.pjv.model.RoadBike;
import cz.cvut.fel.pjv.model.Bicycle;

public class RoadBikeService extends BasicService {
    @Override
    public void accept(RoadBike rb) {
        System.out.println("fixing RoadBike");
    }
}
