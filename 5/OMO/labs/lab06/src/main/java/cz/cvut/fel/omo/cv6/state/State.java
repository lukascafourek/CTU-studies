package cz.cvut.fel.omo.cv6.state;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 * <p>
 * A common state ancestor for all traffic light states.
 */
public abstract class State {

    /**
     * Context interface that allows change state of context from another state
     */
    protected Context context;

    /**
     * Time of life of the state. It allows to survive some predefined period before it changes to next state
     */
    protected int period;

    /**
     * Color of the traffic light for the state
     */
    protected Color color;

    /**
     * Common constructor for the states. It requires context / for this case a TrafficLight
     */
    public State(Context context) {
        this.context = context;
    }

    /**
     * Returns a color that represents a state
     *
     * @return RED, ORANGE, GREEN and NO_COLOR if the traffic light is out of order
     */
    public Color getColor() {
        return color;
    }

    /**
     * Simulates a control clock to change states of traffic light
     */
    public void timeLapseTick() {
        period--;
        if (period == 0) changeToNextState();
    }

    /**
     * Set the next state to the context
     */
    abstract protected void changeToNextState();

    /**
     * Returns the context object
     *
     * @return Traffic light object
     */
    public Context getContext() {
        return context;
    }

    /**
     * Set the context object
     */
    public void setContext(Context context) {
        this.context = context;
    }

    /**
     * Period getter. Get current time to live of the state
     */
    public int getPeriod() {
        return period;
    }

    /**
     * Period setter
     */
    public void setPeriod(int period) {
        this.period = period;
    }
}
