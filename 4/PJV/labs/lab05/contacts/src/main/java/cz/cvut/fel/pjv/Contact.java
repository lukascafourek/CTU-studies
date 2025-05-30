package cz.cvut.fel.pjv;

import java.io.Serializable;

public class Contact implements Comparable<Contact>, Serializable {
  private final String name;
  private final String surname;
  private final int birthYear;

  public Contact(String name, String surname, int birthYear) throws InvalidBirthYearException {
    this.name = name;
    this.surname = surname;
    this.birthYear = birthYear;
    if (birthYear < 1900) {
      throw new InvalidBirthYearException("ERROR: invalid birth year for " + name + " " + surname);
    }
  }

  @Override
  public String toString() {
    return "Contact{" +
        "name='" + name + '\'' +
        ", surname='" + surname + '\'' +
        ", birthYear=" + birthYear +
        "}\n";
  }

  @Override
  public int compareTo(Contact o) {
    return birthYear - o.birthYear;
  }

  public String getSurname() {
    return surname;
  }
}
