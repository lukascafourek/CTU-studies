package cz.cvut.fel.pjv;

import java.io.*;

public class Main {

  private static final String[] RECORDS = {
      "Josef Stefan 1963",
      "Pavlína Blaniková 1987",
      "Oldřich Burda 1988",
      "St5anislav Hons 198",
      "Jan Kužílek 1969",
      "Monika Pražáková 1972",
      "Drahoslava Čermáková 1988",
      "Bořivoj Hrubeš 1984",
      "Adela Rjašková 1989",
      "Eliška Partinglová 1992",
      "Lucie Drábková 1964",
      "Dagmar Hašková 1965",
      "Petr Burda 1978",
      "Alena Denková 1982",
      "Jana Králová 1974",
      "Dominik Hradečný 1976",
      "Karel Bůžek 1987",
      "Kryštof Janíček 1994",
      "Jitka Kracíková 1997",
      "Vladimír Křivan 2001"
  };


  public static void main(String[] args) {
    ContactList contactList = new ContactList();
    for (String record : RECORDS) {
      String[] items = record.split(" ");
      try{
        Contact contact =  new Contact(items[0], items[1],Integer.parseInt(items[2]));
        contactList.addContact(contact);
      }
      catch (InvalidBirthYearException e){
        System.out.println("Error: Invalid birthyear: " + e.getMessage());
      }
    }
    System.out.println(contactList);
    contactList.sortByBirthYear();
    System.out.println(contactList);
    contactList.sortBySurname();
    System.out.println(contactList);

    try (
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("contacts.bin"));
        ObjectInputStream ois = new ObjectInputStream(new FileInputStream("contacts.bin"));
    ) {
      oos.writeObject(contactList);

      ContactList newContactList = (ContactList) ois.readObject();
      System.out.println("--------------------");
      System.out.println(newContactList);

    } catch (IOException | ClassNotFoundException e) {
      System.err.println("Chyba: " + e.getMessage());
    }


  }
}