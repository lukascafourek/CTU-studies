/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.pjv.model;

import java.lang.reflect.Array;
import java.util.ArrayList;

/**
 *
 * @author rudos
 */
public class ServiceBook {
    private ArrayList<String> serviceRecords;
    Car car;

    public ServiceBook(Car car) {
        this.car = car;
        serviceRecords = new ArrayList<String>();
        car.setServiceBook(this);
        //car.serviceBook = this;
        //car.engine = new Engine("super");

    }

    public String getRecords() {
        return String.join(" ", this.serviceRecords);
    }

    public Car getCar() {
        return this.car;
    }

    public void addRecord(String record) {
        this.serviceRecords.add(record);
    }
}
