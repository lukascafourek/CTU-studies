package cz.cvut.fel.omo.cv6.strategy;

import cz.cvut.fel.omo.cv6.Street;
import cz.cvut.fel.omo.cv6.TrafficLight;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 * <p>
 * the strategy used for traffic control during morning hours
 */
public class MorningStrategy extends Strategy {

    private int time = 0;

    public MorningStrategy(Street street) {
        super(street);
    }

    @Override
    public void controlTraffic() {
        int counter = 0;

        for (TrafficLight light : street.getLights()) {
            if (time - counter * lightDistance == 0) {
                light.startGoSequence();
            } else {
                light.timeLapseTick();
            }
            counter++;
        }
        time++;
    }

}
