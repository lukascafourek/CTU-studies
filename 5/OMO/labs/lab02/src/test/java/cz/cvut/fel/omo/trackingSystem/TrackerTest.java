package cz.cvut.fel.omo.trackingSystem;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by kuki on 22/09/2017.
 */
public class TrackerTest {

    @Test
    public void getTrackerMileage_200TrackerMileage_assertEquals() {
        Vehicle car = new Vehicle("Volvo", "VINOKLJ15",  879);
        Tracker tracker = new Tracker(1);
        tracker.attachTracker(car);
        assertEquals(0, tracker.getTrackerMileage());
        car.drive(200);
        assertEquals(200, tracker.getTrackerMileage());
    }
}
