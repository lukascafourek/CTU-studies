package cz.cvut.fel.pjv.cars.model;

import java.util.UUID;

public class Car {
  private final String manufacturer;
  private final String modelName;
  private final int year;
  private final UUID vinCode;
  private final Engine engine;
  private ServiceBook serviceBook;
  private static int carCounter = 0;
  
  public static int getNumberOfExistingCars() {
    return carCounter;
  }

  public Car(String manufacturer, String modelName, int year, String motortype) {
    this.manufacturer = manufacturer;
    this.modelName = modelName;
    this.year = year;
    vinCode = UUID.randomUUID();
    engine = new Engine(motortype);
    carCounter++;
  }

  public String getManufacturer() {
    return manufacturer;
  }

  public String getModelName() {
    return modelName;
  }

  public String getEngineType(){
    return engine.getType();
  }

  public String getServiceBook() {
    return serviceBook.getRecords();
  }

  public void setServiceBook(ServiceBook serviceBook) {
    this.serviceBook = serviceBook;
  }
  
  @Override
  public String toString() {
    return  manufacturer + " " + modelName + " year " + year + " VIN: " + vinCode;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    Car car = (Car) o;

    return vinCode.equals(car.vinCode);
  }

  @Override
  public int hashCode() {
    return vinCode.hashCode();
  }
}
