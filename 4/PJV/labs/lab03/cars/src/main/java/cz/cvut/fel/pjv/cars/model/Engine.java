package cz.cvut.fel.pjv.cars.model;

import java.util.Objects;

public class Engine {
  private final String type;

  public Engine(String type) {
    this.type = type;
  }

  public String getType() {
    return type;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    Engine engine = (Engine) o;

    return Objects.equals(type, engine.type);
  }

  @Override
  public int hashCode() {
    return type != null ? type.hashCode() : 0;
  }
}
