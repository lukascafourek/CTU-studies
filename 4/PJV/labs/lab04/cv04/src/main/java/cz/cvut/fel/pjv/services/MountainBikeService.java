package cz.cvut.fel.pjv.services;

import cz.cvut.fel.pjv.model.MountainBike;
import cz.cvut.fel.pjv.model.Bicycle;

public class MountainBikeService extends BasicService {
    @Override
    public void accept(MountainBike mb) {
        System.out.println("fixing MountainBike");
    }
}
