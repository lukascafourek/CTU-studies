package cz.cvut.fel.omo.cv6.strategy;
// TODO - TO BE IMPLEMENTED

import cz.cvut.fel.omo.cv6.Street;
import cz.cvut.fel.omo.cv6.TrafficLight;

public class EveningStrategy extends Strategy {

    private int time = 0;

    public EveningStrategy(Street street) {
        super(street);
    }

    @Override
    public void controlTraffic() {
        int counter = street.getLights().size() - 1;

        for (TrafficLight light : street.getLights()) {
            if (time - counter * lightDistance == 0) {
                light.startGoSequence();
            } else {
                light.timeLapseTick();
            }
            counter--;
        }
        time++;
    }
}