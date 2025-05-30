package cz.cvut.fel.pjv.cars.data;

import cz.cvut.fel.pjv.cars.model.Car;

public class RaceResult {
  private final Car car;
  private final TimeSpan timeSpan;

  public RaceResult(Car car, TimeSpan timeSpan) {
    this.car = car;
    this.timeSpan = timeSpan;
  }
  
}
