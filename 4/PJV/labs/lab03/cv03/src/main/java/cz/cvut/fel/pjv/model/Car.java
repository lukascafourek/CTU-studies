/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.pjv.model;

import java.util.UUID;

/**
 *
 * @author rudos
 */
public class Car {

    static private int numberOfCars = 0;
    private String manufacturer;
    private String modelName;
    private int year;
    private UUID vinCode;
    public Engine engine;
    public ServiceBook serviceBook;

    public Car(String manufacturer, String modelName, Integer year, String motortype) {
        this.manufacturer = manufacturer;
        this.modelName = modelName;
        this.year = year;
        this.vinCode = UUID.randomUUID();
        this.engine = new Engine(motortype);
//        this.serviceBook = new ServiceBook(this);
    }

    static public int getNumberOfExistingCars() {
        numberOfCars += 1;
        return numberOfCars;
    }

    public String getManufacturer() {
        return this.manufacturer;
    }
    public String getEngineType(){
        return this.engine.getType();
    }

    public String getModelName() {
        return this.modelName;
    }

    public String getServiceBook() {
        return this.serviceBook.getRecords();
    }

    public void setServiceBook(ServiceBook serviceBook) {
        this.serviceBook = serviceBook;
    }

    @Override
    public String toString() {
        return this.manufacturer + " " + this.modelName + " year " + this.year + " VIN: " + this.vinCode + ".";
    }

    @Override
    public boolean equals(Object obj) {
        return this.vinCode.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.vinCode.hashCode();
    }

}
