package cz.cvut.fel.omo.cv6;

import cz.cvut.fel.omo.cv6.state.*;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 */
public class TrafficLight implements Context {

    /**
     * Current state of traffic light
     */
    private State state = new Stop(this);

    /**
     * State setter
     *
     * @param state A new state
     */
    public void setState(State state) {
        this.state = state;
    }

    /**
     * State getter
     *
     * @return Current state
     */
    public State getState() {
        return state;
    }

    /**
     * Returns a current color of light
     */
    Color getLightColor() {
        return state.getColor();
    }

    /**
     * Begin a sequence of ORANGE - GREEN - ORANGE - RED
     */
    public void startGoSequence() {
        setState(new Prepare(this));
    }

    /**
     * Simulates a clock impulse that control traffic lights
     */
    public void timeLapseTick() {
        state.timeLapseTick();
    }

}


