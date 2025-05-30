package cz.cvut.fel.omo.cv6;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.time.LocalDateTime;

public class StreetTest {

    Street street;
    ByteArrayOutputStream out;

    @Before
    public void beforeEachTest() {
        out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
    }

    @After
    public void cleanUpStreams() {
        System.setOut(null);
        System.setErr(null);
    }

    @Test
    public void runTrafficLights_simulateControl_theTrafficLightsFollowTheMorningStrategy() {

        // arrange
        LocalDateTime date = LocalDateTime.of(2017, 11, 6, 6, 0);
        street = new Street(5, 0, date);
        String expectedOutput1 = "0 ORANGE RED RED RED RED";
        String expectedOutput2 = "2 GREEN RED RED RED RED";
        String expectedOutput3 = "5 GREEN ORANGE RED RED RED";
        String expectedOutput4 = "13 ORANGE GREEN GREEN RED RED";
        String expectedOutput5 = "14 RED GREEN GREEN RED RED";
        String expectedOutput6 = "28 RED RED RED ORANGE GREEN";

        // act
        street.runTrafficLights();

        // assert
        String actualOut = out.toString();
        Assert.assertEquals(expectedOutput1, actualOut.substring(actualOut.indexOf("0 "), actualOut.indexOf("0 ") + 24));
        Assert.assertEquals(expectedOutput2, actualOut.substring(actualOut.indexOf("2 "), actualOut.indexOf("2 ") + 23));
        Assert.assertEquals(expectedOutput3, actualOut.substring(actualOut.indexOf("5 "), actualOut.indexOf("5 ") + 26));
        Assert.assertEquals(expectedOutput4, actualOut.substring(actualOut.indexOf("13 "), actualOut.indexOf("13 ") + 29));
        Assert.assertEquals(expectedOutput5, actualOut.substring(actualOut.indexOf("14 "), actualOut.indexOf("14 ") + 26));
        Assert.assertEquals(expectedOutput6, actualOut.substring(actualOut.indexOf("28 "), actualOut.indexOf("28 ") + 27));
    }


    @Test
    public void runTrafficLights_simulateControl_theTrafficlightsFollowTheEveningStrategy() {

        // arrange
        LocalDateTime date = LocalDateTime.of(2017, 11, 6, 18, 0);
        street = new Street(5, 0, date);
        String expectedOutput1 = "0 RED RED RED RED ORANGE";
        String expectedOutput2 = "2 RED RED RED RED GREEN";
        String expectedOutput3 = "5 RED RED RED ORANGE GREEN";
        String expectedOutput4 = "13 RED RED GREEN GREEN ORANGE";
        String expectedOutput5 = "14 RED RED GREEN GREEN RED";
        String expectedOutput6 = "28 GREEN ORANGE RED RED RED";

        // act
        street.runTrafficLights();

        // assert
        String actualOut = out.toString();
        Assert.assertEquals(expectedOutput1, actualOut.substring(actualOut.indexOf("0 "), actualOut.indexOf("0 ") + 24));
        Assert.assertEquals(expectedOutput2, actualOut.substring(actualOut.indexOf("2 "), actualOut.indexOf("2 ") + 23));
        Assert.assertEquals(expectedOutput3, actualOut.substring(actualOut.indexOf("5 "), actualOut.indexOf("5 ") + 26));
        Assert.assertEquals(expectedOutput4, actualOut.substring(actualOut.indexOf("13 "), actualOut.indexOf("13 ") + 29));
        Assert.assertEquals(expectedOutput5, actualOut.substring(actualOut.indexOf("14 "), actualOut.indexOf("14 ") + 26));
        Assert.assertEquals(expectedOutput6, actualOut.substring(actualOut.indexOf("28 "), actualOut.indexOf("28 ") + 27));
    }

}
