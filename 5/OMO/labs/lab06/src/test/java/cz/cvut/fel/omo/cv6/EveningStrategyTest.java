package cz.cvut.fel.omo.cv6;

import cz.cvut.fel.omo.cv6.strategy.EveningStrategy;
import cz.cvut.fel.omo.cv6.strategy.Strategy;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.time.LocalDateTime;

public class EveningStrategyTest {

    Street street;
    ByteArrayOutputStream out;

    @Before
    public void beforeEachTest() {
        out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        street = new Street(5, 0, LocalDateTime.now());

    }

    @After
    public void cleanUpStreams() {
        System.setOut(null);
        System.setErr(null);
    }

    @Test
    public void controlTraffic_simulateStreetWith5TrafficLights_greenWaveFromRightToLeft() {
        // arrange
        String expectedState = "RED RED RED RED ORANGE ";

        // act
        Strategy strategy = new EveningStrategy(street);
        strategy.controlTraffic();

        // assert
        Assert.assertEquals(expectedState, street.toString());
    }

}
