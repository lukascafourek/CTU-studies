package cz.cvut.fel.pjv.cars.data;

import cz.cvut.fel.pjv.cars.model.Car;

public class CarListNode {
  private final Car car;
  private CarListNode next;

  public CarListNode(Car car) {
    this.car = car;
    next = null;
  }

  public Car getCar() {
    return car;
  }

  public CarListNode getNext() {
    return next;
  }

  public void setNext(CarListNode next) {
    this.next = next;
  }
}
