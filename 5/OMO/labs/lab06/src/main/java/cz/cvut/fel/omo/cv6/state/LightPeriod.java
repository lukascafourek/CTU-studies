package cz.cvut.fel.omo.cv6.state;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 * <p>
 * The time of shining of specific color. RED - 10 time units, ORANGE - 2 time units, GREEN - 10 time unis
 */
public enum LightPeriod {

    RED_LIGHT_PERIOD(20),
    ORANGE_LIGHT_PERIOD(2),
    GREEN_LIGHT_PERIOD(10);

    private final int value;

    LightPeriod(int value) {
        this.value = value;
    }

    public int getValue() {
        return this.value;
    }

}
