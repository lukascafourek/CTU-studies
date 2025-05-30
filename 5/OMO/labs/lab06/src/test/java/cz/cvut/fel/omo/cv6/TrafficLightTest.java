package cz.cvut.fel.omo.cv6;

import cz.cvut.fel.omo.cv6.state.Color;
import cz.cvut.fel.omo.cv6.state.LightPeriod;
import cz.cvut.fel.omo.cv6.state.Prepare;
import cz.cvut.fel.omo.cv6.state.Stop;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TrafficLightTest {

    TrafficLight light;

    @Before
    public void setUp() {
        light = new TrafficLight();
    }

    @Test
    public void setState_setStopState_lightTrafficColorIsRed() {
        // arrange
        Color expectedColor = Color.RED;
        //act
        light.setState(new Stop(light));
        // assert
        Assert.assertEquals(expectedColor, light.getLightColor());
    }

    @Test
    public void setState_setPrepareState_lightTrafficColorIsOrange() {
        // arrange
        Color expectedColor = Color.ORANGE;
        //act
        light.setState(new Prepare(light));
        // assert
        Assert.assertEquals(expectedColor, light.getLightColor());
    }


    @Test
    public void timeLapseTick_simulateTrafficLightOperation_colorIsChangingBySetLightColorPeriod() {
        // arrange
        light.setState(new Prepare(light));

        // act
        for (int i = 0; i < LightPeriod.ORANGE_LIGHT_PERIOD.getValue(); i++) {
            light.timeLapseTick();
        }

        // assert
        Assert.assertEquals(Color.GREEN, light.getLightColor());

        // act
        for (int i = 0; i < LightPeriod.GREEN_LIGHT_PERIOD.getValue(); i++) {
            light.timeLapseTick();
        }

        // assert
        Assert.assertEquals(Color.ORANGE, light.getLightColor());

        // act
        for (int i = 0; i < LightPeriod.ORANGE_LIGHT_PERIOD.getValue(); i++) {
            light.timeLapseTick();
        }

        // assert
        Assert.assertEquals(Color.RED, light.getLightColor());
    }

}
