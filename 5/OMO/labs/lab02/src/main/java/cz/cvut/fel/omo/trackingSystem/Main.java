package cz.cvut.fel.omo.trackingSystem;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by kuki on 22/09/2017.
 */

public class Main {

    public static void main(String[] args) {

        // 1. Initialize company car park
        List<Vehicle> carPark = new ArrayList<>();
        GpsTrackingSystem companySpy = new GpsTrackingSystem();
        carPark.add(new Vehicle("Volvo", "VINOKLJ15",  879));
        carPark.add(new Vehicle("Volvo", "OINNLKA48", 9080));
        carPark.add(new Vehicle("Volvo", "OPWAKMKL8", 1280));



        // Attaching tracking devices to the car park
        companySpy.attachTrackingDevices(carPark);

        // Counting initial traveled distance with trackers
        companySpy.generateMonthlyReport();

        // Busy month filled with business travelling
        carPark.forEach(car->car.drive(100));

        // End of another month, generate new report
        companySpy.generateMonthlyReport();

        // Busy month filled with business travelling
        carPark.forEach(car->car.drive(300));

        // End of another month, generate new report
        companySpy.generateMonthlyReport();
    }
}
