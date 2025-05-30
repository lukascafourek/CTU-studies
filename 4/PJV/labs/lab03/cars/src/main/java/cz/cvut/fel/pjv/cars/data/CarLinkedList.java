package cz.cvut.fel.pjv.cars.data;

import cz.cvut.fel.pjv.cars.model.Car;

public class CarLinkedList {
  private CarListNode head = null; // make it explicit
  private int count = 0;
  
  public void addCar(Car car) {
    CarListNode newNode = new CarListNode(car);
    count++;
    if (head == null) {
      head = newNode;
      return;
    }
    CarListNode tmp = head;
    while (tmp.getNext() != null) {
      tmp = tmp.getNext();
    }
    tmp.setNext(newNode);
  }
  
  public Car[] toArray() {
    Car[] array = new Car[count];
    int i = 0;
    CarListNode tmp = head;
    while (tmp != null) {
      array[i++] = tmp.getCar();
      tmp = tmp.getNext();
    }
    return array;
  }
  
}
