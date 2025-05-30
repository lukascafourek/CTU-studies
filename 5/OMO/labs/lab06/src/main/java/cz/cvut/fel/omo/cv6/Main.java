package cz.cvut.fel.omo.cv6;

import java.time.LocalDateTime;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 */
public class Main {

    static final int NUMBER_OF_TRAFFIC_LIGHTS = 5;

    public static void main(String[] args) {
        Street street = new Street(NUMBER_OF_TRAFFIC_LIGHTS, 10, LocalDateTime.now());
        street.runTrafficLights();
    }
}
