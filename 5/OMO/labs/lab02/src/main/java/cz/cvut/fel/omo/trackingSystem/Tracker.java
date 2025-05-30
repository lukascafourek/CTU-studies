package cz.cvut.fel.omo.trackingSystem;

/**
 * Tracker is device installed into company vehicles, connected to car computer in order to obtain necessary data.
 */
public class Tracker {

    private Vehicle currentVehicle;
    private int innerMemory;
    private final int trackerId;

    public Tracker(int trackerId) {
        this.trackerId = trackerId;
    }

    public Vehicle getCurrentVehicle() {
        return currentVehicle;
    }

    public void attachTracker(Vehicle vehicle) {
        currentVehicle = vehicle;
        innerMemory = vehicle.getMileage();
    }

    public int getTrackerMileage() {
        return currentVehicle.getMileage() - innerMemory;
    }

    public void resetTrackerMileage() {
        innerMemory = currentVehicle.getMileage();
    }

    @Override
    public String toString() {
        return "Tracker_" + trackerId + ", attached to " + currentVehicle;
    }
}
