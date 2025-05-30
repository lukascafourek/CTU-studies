package cz.cvut.fel.omo.cv6;

import cz.cvut.fel.omo.cv6.strategy.MorningStrategy;
import cz.cvut.fel.omo.cv6.strategy.Strategy;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.time.LocalDateTime;

public class MorningStrategyTest {


    Street street;
    ByteArrayOutputStream out;
    Strategy strategy;

    @Before
    public void beforeEachTest() {
        out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        street = new Street(5, 0, LocalDateTime.now());
        strategy = new MorningStrategy(street);
    }

    @After
    public void cleanUpStreams() {
        System.setOut(null);
        System.setErr(null);
    }

    @Test
    public void controlTraffic_simulateStreetWith5TrafficLightsFirstStep_greenWaveFromLeftToRight() {
        // arrange
        String expectedState = "ORANGE RED RED RED RED ";

        // act
        strategy.controlTraffic();

        // assert
        Assert.assertEquals(expectedState, street.toString());
    }

    @Test
    public void controlTraffic_simulateStreetWith5TrafficLightsFirstSecondStep_greenWaveFromLeftToRight() {
        // arrange
        String expectedState = "ORANGE RED RED RED RED ";

        // act
        strategy.controlTraffic();
        strategy.controlTraffic();

        // assert
        Assert.assertEquals(expectedState, street.toString());
    }

    @Test
    public void controlTraffic_simulateStreetWith5TrafficLightsThirdSecondStep_greenWaveFromLeftToRight() {
        // arrange
        String expectedState = "GREEN RED RED RED RED ";

        // act
        strategy.controlTraffic();
        strategy.controlTraffic();
        strategy.controlTraffic();

        // assert
        Assert.assertEquals(expectedState, street.toString());
    }

}
