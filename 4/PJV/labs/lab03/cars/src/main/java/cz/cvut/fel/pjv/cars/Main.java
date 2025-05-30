package cz.cvut.fel.pjv.cars;

import cz.cvut.fel.pjv.cars.data.TimeSpan;
import cz.cvut.fel.pjv.cars.model.Car;
import cz.cvut.fel.pjv.cars.model.ServiceBook;

public class Main {

  public static void main(String[] args) {
//    Car car1 = new Car("Volkswagen", "Polo", 2010);
//    System.out.println(car1);
//    System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
//    Car car2 = new Car("Chevrolet", "Corvette", 1980);
//    System.out.println(car2);
//    System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
    Car car1 = new Car("Volkswagen", "Polo", 2010, "AKK");
    ServiceBook serviceBook1 = new ServiceBook(car1);
    serviceBook1.addRecord("První servisní prohlídka.");
    System.out.println(car1);
    System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
    System.out.printf("Servisní záznamy %s %s:\n%s\n", car1.getManufacturer(), car1.getModelName(), car1.getServiceBook());
    Car car2 = new Car("Chevrolet", "Corvette", 1980, "LS7");
    ServiceBook serviceBook2 = new ServiceBook(car2);
    serviceBook2.addRecord("První servisní prohlídka.");
    serviceBook2.addRecord("Porucha motoru.");
    System.out.println(car2);
    System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
    System.out.printf("Servisní záznamy %s %s:\n%s\n", car2.getManufacturer(), car2.getModelName(), car2.getServiceBook());
    TimeSpan t1 = new TimeSpan(0, 0, 130);
    System.out.println("T1: " + t1);
    t1 = new TimeSpan(130);
    System.out.println("T1: " + t1);
    TimeSpan t2 = new TimeSpan(t1).add(70);
    System.out.println("T1: " + t1);
    System.out.println("T2: " + t2);
    System.out.printf("Časy se %s.\n", t1.equals(t2) ? "rovnají" : "nerovnají");
    t1 = t1.add(70);
    System.out.println("T1: " + t1);
    System.out.println("T2: " + t2);
    System.out.printf("Časy se %s.\n", t1.equals(t2) ? "rovnají" : "nerovnají");
  }
  
}
