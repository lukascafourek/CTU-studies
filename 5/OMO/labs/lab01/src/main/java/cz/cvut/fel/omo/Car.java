package cz.cvut.fel.omo;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

public class Car {

    private Date date;

    private String color;

    private List<Wheel> wheels = new ArrayList<>();

    private static int count = 0;

    private static final Logger logger = Logger.getLogger(Car.class.getName());

    public Car(int numOfWheels, int diameter) {
        date = new Date();
        color = String.valueOf(numOfWheels);
        count++;
        for (int i = 0; i < numOfWheels; i++) {
            wheels.add(new Wheel(diameter));
            logger.info("Added Wheel " + i + ": " + wheels.get(i).toString());
        }
        logger.info(this.toString());
    }

    public Date getDate() {
        return date;
    }

    public String getColor() {
        return color;
    }

    public List<Wheel> getWheels() {
        return wheels;
    }

    @Override
    public String toString() {
        return "Car{" +
                "rok_vyroby=" + date +
                ", barva='" + color + '\'' +
                ", wheels=" + wheels +
                '}';
    }
}
