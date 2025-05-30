package cz.cvut.fel.omo.trackingSystem;

/**
 * Class Vehicle represents a single car in company car park.
 */
public class Vehicle {

    private final String manufacturer;
    private int mileage;
    private final String VINCode;

    public Vehicle(String manufacturer, String VINCode, int mileage) {
        this.manufacturer = manufacturer;
        this.VINCode = VINCode;
        this.mileage = mileage;
    }

    public void drive(int miles) {
        mileage += miles;
    }

    public String getManufacturer() {
        return manufacturer;
    }

    public int getMileage() {
        return mileage;
    }

    public String getVINCode() {
        return VINCode;
    }

    @Override
    public String toString() {
        return manufacturer + ", " + VINCode;
    }
}
