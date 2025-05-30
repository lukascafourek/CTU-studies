/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.pjv;

import cz.cvut.fel.pjv.model.Car;
import cz.cvut.fel.pjv.model.ServiceBook;
import cz.cvut.fel.pjv.data.TimeSpan;

/**
 *
 * @author rudos
 */
public class MainSecond {
    
    private static void firstPartTest() {
        Car car1 = new Car("Volkswagen", "Polo", 2010, "I4");
        System.out.println(car1);
        System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
        Car car2 = new Car("Chevrolet", "Corvette", 1980, "V8");
        System.out.println(car2);
        System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
    }
    
    public static void secondPartTest() {

        Car car1 = new Car("Volkswagen", "Polo", 2010, "AKK");
        ServiceBook serviceBook1 = new ServiceBook(car1);
//        car1.setServiceBook(serviceBook1);
        serviceBook1.addRecord("První servisní prohlídka.");
        System.out.println(car1);
        System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
        System.out.printf("Servisní záznamy %s %s:\n%s\n", 
                car1.getManufacturer(), car1.getModelName(), car1.getServiceBook());
        Car car2 = new Car("Chevrolet", "Corvette", 1980, "LS7");
        ServiceBook serviceBook2 = new ServiceBook(car2);
//        car2.setServiceBook(serviceBook2);
        serviceBook2.addRecord("První servisní prohlídka.");
        serviceBook2.addRecord("Porucha motoru.");
        System.out.println(car2);
        System.out.println("Počet aut: " + Car.getNumberOfExistingCars());
        System.out.printf("Servisní záznamy %s %s:\n%s\n", 
                car2.getManufacturer(), car2.getModelName(), car2.getServiceBook());

    }
    
            
   public static void thirdPartTest() {
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
        /*Ukazka zretezeni*/
      /*
        t1.addHours(2).addMinutes(45).add(5);
        System.out.println("Ukazka zretezeni T1: " + t1);
      */
    }
    
    
    public static void main(String[] args) {
        //firstPartTest();
        //secondPartTest();
        thirdPartTest();
    }
}
