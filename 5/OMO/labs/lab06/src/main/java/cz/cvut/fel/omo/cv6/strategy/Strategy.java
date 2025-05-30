package cz.cvut.fel.omo.cv6.strategy;

import cz.cvut.fel.omo.cv6.Street;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 * <p>
 * Common strategy class that defines execution method providing the strategy implementation
 */
public abstract class Strategy {

    /**
     * data about traffic lights
     */
    protected Street street;

    /**
     * distance of traffic lights
     */
    protected int lightDistance = 5;


    public Strategy(Street street) {
        this.street = street;
    }

    /**
     * the execution strategy method
     */
    public abstract void controlTraffic();

}
