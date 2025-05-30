package cz.cvut.fel.pjv.cars.model;

import java.util.Arrays;

public class ServiceBook {
    private final int size = 10;
    private final String[] serviceRecords = new String[size];
    private final Car car;

    public ServiceBook(Car car) {
        this.car = car;
        car.setServiceBook(this);
    }

    public String getRecords() {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < serviceRecords.length; i++) {
            if (serviceRecords[i] != null && i != serviceRecords.length - 1) {
                result.append(serviceRecords[i]).append(" ");
            }
        }
        return result.toString();
    }

    public Car getCar() {
        return car;
    }

    public void addRecord(String record) {
        int i = 0;
        while(i < serviceRecords.length && serviceRecords[i] != null) {
            i++;
        }
        if (i < serviceRecords.length) {
            serviceRecords[i] = record;
        }
    }

    @Override
    public String toString() {
        return "ServiceBook{" +
                "serviceRecords=" + Arrays.toString(serviceRecords) +
                '}';
    }
}
