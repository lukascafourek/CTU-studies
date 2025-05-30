package cz.cvut.fel.omo;

import java.util.logging.Logger;

public class Main {

    private static Logger logger = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
        createTruck(6, 50);
        createCar(4, 17);
    }

    public static void createTruck(int numOfWheels, int diameter) {
        Car truck = new Car(numOfWheels, diameter);
    }

    public static void createCar(int numOfWheels, int diameter) {
        Car car = new Car(numOfWheels, diameter);
    }
}
