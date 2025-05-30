package cz.cvut.fel.pjv;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

public class SurnameComparator implements Comparator<Contact> {
  @Override
  public int compare(Contact o1, Contact o2) {
    Collator coll = Collator.getInstance(Locale.forLanguageTag("cs_CZ"));
    return coll.compare(o1.getSurname(), o2.getSurname());
//    return o1.getSurname().compareTo(o2.getSurname());
  }
}
