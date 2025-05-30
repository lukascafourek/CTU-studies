package cz.cvut.fel.omo.trackingSystem;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by kuki on 22/09/2017.
 */
public class VehicleTest {

    @Test
    public void drive_plus200Mileage_assertEquals() {
        Vehicle car = new Vehicle("Volvo", "VINOKLJ15",  879);
        car.drive(200);
        assertEquals(1079, car.getMileage());
    }
}
