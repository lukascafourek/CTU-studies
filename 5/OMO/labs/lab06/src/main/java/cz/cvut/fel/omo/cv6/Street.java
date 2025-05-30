package cz.cvut.fel.omo.cv6;

import cz.cvut.fel.omo.cv6.strategy.EveningStrategy;
import cz.cvut.fel.omo.cv6.strategy.MorningStrategy;
import cz.cvut.fel.omo.cv6.strategy.Strategy;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 * <p>
 * The class simulates a street with sequence of traffic lights. The lights are controlled by selected strategy - MorningStrategy before 12 p.m. and EvenningStrategy after
 */
public class Street {

    /**
     * control speed of the simulation reasonable values between 0 - for unit tests to 10 - for near real world simulation
     */
    private final int timeSpeed;

    /**
     * time of simulation. Important for appropriate strategy.
     */
    private final LocalDateTime currentTime;

    /**
     * List of sequence of traffic lights
     */
    private final List<TrafficLight> lights = new ArrayList<>();

    /**
     * Current strategy to control traffic
     */
    private Strategy strategy;


    /**
     * Creates and switches on the sequence of traffic lights
     */
    public Street(int numberOfTrafficLights, int timeSpeed, LocalDateTime currentTime) {

        // prepare traffic lights
        for (int i = 0; i < numberOfTrafficLights; i++) {
            lights.add(new TrafficLight());
        }

        this.timeSpeed = timeSpeed;
        this.currentTime = currentTime;

    }


    /**
     * Select an appropriate strategy to control traffic lights and simulates clock impulses that run the traffic light logic
     */
    public void runTrafficLights() {

        // choose the appropriate strategy
        // before 12pm use morning strategy, evening strategy use in the afternoon and evening
        //  TODO - TO BE IMPLEMENTED
        if (this.currentTime.getHour() < 12) {
            strategy = new MorningStrategy(this);
        } else {
            strategy = new EveningStrategy(this);
        }

        // simulates operating of traffic lights for 1000 time units using the strategy
        for (int time = 0; time < 1000; time++) {
            strategy.controlTraffic();

            // print the traffic lights colors
            System.out.print(time);
            System.out.print(" ");
            System.out.println(this);

            // wait for a while
            try {
                Thread.sleep(timeSpeed * 100L);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * Getter
     *
     * @return List of traffic lights
     */
    public List<TrafficLight> getLights() {
        return lights;
    }

    /**
     * Getter
     *
     * @return Current strategy
     */
    public Strategy getStrategy() {
        return strategy;
    }

    /**
     * Setter
     *
     * @param strategy new strategy
     */
    public void setStrategy(Strategy strategy) {
        this.strategy = strategy;
    }

    /**
     * String representation of sequence of traffic lights
     *
     * @return String representation of current lights color
     */
    public String toString() {
        StringBuilder output = new StringBuilder();
        for (TrafficLight light : lights) {
            output.append(light.getLightColor());
            output.append(" ");
        }
        return output.toString();
    }

}
