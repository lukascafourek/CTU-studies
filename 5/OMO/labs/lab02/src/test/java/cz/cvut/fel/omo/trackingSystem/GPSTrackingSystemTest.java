package cz.cvut.fel.omo.trackingSystem;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Created by kuki on 22/09/2017.
 */
public class GPSTrackingSystemTest {

    @Test
    public void attachTrackingDevices_3TrackersAttached_AssertEquals() {
        List<Vehicle> carPark = new ArrayList<>();
        GpsTrackingSystem companySpy = new GpsTrackingSystem();
        carPark.add(new Vehicle("Volvo", "VINOKLJ15",  879));
        carPark.add(new Vehicle("Volvo", "OINNLKA48", 9080));
        carPark.add(new Vehicle("Volvo", "OPWAKMKL8", 1280));
        assertEquals(1, GpsTrackingSystem.getCounter());
        companySpy.attachTrackingDevices(carPark);
        assertEquals(4, GpsTrackingSystem.getCounter());
        assertEquals(3, companySpy.getActiveTrackers().size());
    }
}
