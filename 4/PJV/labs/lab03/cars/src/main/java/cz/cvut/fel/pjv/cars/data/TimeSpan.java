package cz.cvut.fel.pjv.cars.data;

import java.util.Objects;

public class TimeSpan {
  private int hour;
  private int minute;
  private int second;

  public TimeSpan() {
    this.hour = 0;
    this.minute = 0;
    this.second = 0;
  }
  public TimeSpan(int second) {
    this.second = second % 60;
    this.minute = second / 60;
    this.hour = this.minute / 60;
    this.minute = this.minute % 60;
  }
  public TimeSpan(int minute, int second) {
    this.second = second % 60;
    this.minute = second / 60 + minute;
    this.hour = this.minute / 60;
    this.minute = this.minute % 60;
  }
  public TimeSpan(int hour, int minute, int second) {
    this.second = second % 60;
    this.minute = second / 60 + minute;
    this.hour = this.minute / 60 + hour;
    this.minute = this.minute % 60;
  }
  public TimeSpan(TimeSpan timeSpan) {
    this.second = timeSpan.second;
    this.hour = timeSpan.hour;
    this.minute = timeSpan.minute;
  }
  public TimeSpan add(int second) {
    this.second = (this.second + second) % 60;
    this.minute = this.minute + second / 60;
    this.hour = this.minute / 60;
    this.minute = this.minute % 60;
    return this;
  }
  @Override
  public String toString() {
    return "h:"+hour+" m:"+minute+" s:"+second;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    TimeSpan timeSpan = (TimeSpan) o;
    return hour == timeSpan.hour && minute == timeSpan.minute && second == timeSpan.second;
  }

  @Override
  public int hashCode() {
    return Objects.hash(hour, minute, second);
  }
}
