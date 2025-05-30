package cz.cvut.fel.pjv;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class ContactList implements Serializable {
  private final List<Contact> contacts = new ArrayList<>();

  public void addContact (Contact contact){
    contacts.add(contact);
  }

  public void sortByBirthYear() {
    Collections.sort(contacts);
  }

  public void sortBySurname() {
    contacts.sort(new SurnameComparator());
  }

  @Override
  public String toString() {
    return "ContactList{" +
        "contacts=" + contacts +
        '}';
  }
}
