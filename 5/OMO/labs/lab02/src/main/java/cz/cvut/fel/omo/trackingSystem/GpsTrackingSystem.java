package cz.cvut.fel.omo.trackingSystem;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by kuki on 22/09/2017.
 * GpsTrackingSystem class represents the newly introduced tool for gaining control over company car park.
 */
public class GpsTrackingSystem {

    private final List<Tracker> activeTrackers = new ArrayList<>();
    private static int counter = 1;

    public GpsTrackingSystem() {}

    public void attachTrackingDevices(List<Vehicle> vehicles) {
        for (Vehicle vehicle : vehicles) {
            Tracker tracker = new Tracker(counter);
            tracker.attachTracker(vehicle);
            activeTrackers.add(tracker);
            counter++;
        }
    }

    public void generateMonthlyReport() {
        int miles = 0;
        System.out.println("—– GPS Tracking system: Monthly report —–\n" + "Currently active devices:");
        for (Tracker tracker : activeTrackers) {
            miles += tracker.getTrackerMileage();
            System.out.println(tracker);
            tracker.resetTrackerMileage();
        }
        System.out.println("This month traveled distance: " + miles + " km");
    }

    public List<Tracker> getActiveTrackers() {
        return activeTrackers;
    }

    public static int getCounter() {
        return counter;
    }
}
