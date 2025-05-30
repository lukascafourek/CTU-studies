package list;

import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;


/**
 * Obalujici trida pro polozku zretezeneho seznamu
 * @param <T> Datovy typ k ulozeni
 */
class MyListItem<T> {

    T value;
    MyListItem<T> next;
    MyListItem<T> previous;

    public MyListItem(T value, MyListItem<T> previous, MyListItem<T> next){
        this.value = value;
        this.next = next;
        this.previous = previous;
    }

    public MyListItem(T value){
        this(value, null, null);
    }
}

/**
 * Zretezeny seznam
 * @param <T> Datovy typ k ulozeni
 */
public class MyList<T> implements Iterable<T> {

    MyListItem<T> first = null;
    MyListItem<T> last = null;

    /**
     * Vlozi prvek na konec seznamu
     * @param value vkladany prvek
     */
    public void addLast(T value) {
        MyListItem<T> tmp;
        if (last == null) {
            tmp = new MyListItem<T>(value);
            first = tmp;
        } else {
            tmp = new MyListItem<T>(value, last, null);
            last.next = tmp;
        }
        last = tmp;
    }

    /**
     * Vlozi prvek na zacatek seznamu
     * @param value vkladany prvek
     */
    public void addFirst(T value) {
        MyListItem<T> tmp;
        if (first == null){
            tmp = new MyListItem<T>(value);
            last = tmp;
        } else {
            tmp = new MyListItem<T>(value, null, first);
            first.previous = tmp;
        }
        first = tmp;
    }

    /**
     * Vytvori list-iterator pro pruchod seznamem
     * NEMENTE
     * @return instanci list-iteratoru
     */
    public ListIterator<T> listIterator() {
        return new MyIterator<>(this);
    }

    /**
     * Vytvori iterator pro pruchod seznamem
     * NEMENTE
     * @return instanci iteratoru
     * @see #listIterator()
     */
    public Iterator<T> iterator(){
        return listIterator();
    }
}


/**
 * Iterator pro pruchod seznamem
 * @param <T> typ vkladaneho prvku
 */
class MyIterator<T> implements ListIterator<T> {

    MyList<T> list;
    MyListItem<T> rightBox;
    MyListItem<T> leftBox;
    MyListItem<T> temp;
    int index;

    /**
     * @param list odkaz na iterovany seznam
     */
    MyIterator(MyList<T> list){
        this.list = list;
        leftBox = null;
        rightBox = list.first;
        temp = null;
        index = -1;
    }

    /**
     * @return true, pokud je za pozici iteratoru dalsi prvek
     */
    public boolean hasNext() {
        return rightBox != null;
    }

    /**
     * Vrati prvek za pozici iteratoru a posune o jednu pozici dopredu
     * @return dalsi prvek za pozici iteratoru
     * @throws NoSuchElementException pokud takovy prvek neexistuje
     */
    public T next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        index++;
        temp = rightBox;
        rightBox = temp.next;
        leftBox = temp;
        return temp.value;
    }

    /**
     * @return True, pokud je pred pozici iteratoru prvek
     */
    public boolean hasPrevious() {
        return leftBox != null;
    }

    /**
     * Vrati prvek pred iteratorem a posune pozici iteratoru o jednu zpet.
     * @return vraci predchozi prvek pred iteratorem
     * @throws NoSuchElementException pokud takovy prvek neexistuje
     */
    public T previous() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        index--;
        temp = leftBox;
        leftBox = temp.previous;
        rightBox = temp;
        return temp.value;
    }

    /**
     * Vraci pozici prvku, ktery by byl precteny dalsim volanim next.
     * Prvni prvek je 0, posledni n-1. Pokud je iterator za poslednim prvkem
     * (to zahrnuje i prazdny seznam), vrati n (prazdny -> n=0), kde n je
     * pocet prvku seznamu.
     * @see #next()
     * @return pozici prvku, ktery ma byt precten pomoci next
     */
    public int nextIndex() {
        return index + 1;
    }

    /**
     * Vraci pozici prvku, ktery by byl precten pomoci dalsiho volani previous.
     * Prvni prvek je 0, posledni n-1. Pokud je iterator pred prvni prvekem
     * (to zahrnuje i prazdny seznam), vrati -1. n je pocet prvku seznamu.
     * @see #previous()
     * @return pozici prvku, ktery ma byt precten pomoci previous
     */
    public int previousIndex() {
        return index;
    }

    /**
     * Vyjme ze seznamu prvek, ktery byl naposled precten operaci next nebo
     * previous.
     * @see #next()
     * @see #previous()
     * @throws IllegalStateException v pripade, ze nebylo volano next ani previous,
     * nebo byla po nich volana metoda remove nebo add.
     */
    public void remove() {
        if (temp == null) {
            throw new IllegalStateException();
        }
        if (temp.previous == null) {
            list.first = temp.next;
        } else {
            temp.previous.next = temp.next;
        }
        if (temp.next == null) {
            list.last = null;
        } else {
            temp.next.previous = temp.previous;
        }

        leftBox = temp.previous;
        rightBox = temp.next;
        temp = null;
        index--;
    }

    /**
     * Nastavi novou hodnotu prvku, ktery byl naposled precten operaci next nebo
     * previous.
     * @param e nova hodnota prvku
     * @see #next()
     * @see #previous()
     * @throws IllegalStateException v pripade, ze nebylo volano next ani previous,
     * nebo byla po nich volana metoda remove nebo add.
     */
    public void set(T e) {
        if (temp == null) {
            throw new IllegalStateException();
        }
        temp.value = e;
    }

    /**
     * Na misto iteratoru se vlozi novy prvek. Iterator bude ukazovat ZA vlozeny
     * prvek. Tudiz se zvetsi indexy o 1, pripadne volani previous vrati prave
     * vlozeny prvek, hodnotu volani next to neovlivni.
     * @param e hodnota vkladaneho prvku
     */
    public void add(T e) {
        MyListItem<T> prvek=new MyListItem<>(e);
        if (leftBox == null && rightBox == null) {
            list.first = prvek;
            list.last = prvek;
        } else if (rightBox == null) {
            list.last = prvek;
            leftBox.next = prvek;
        } else if (leftBox == null) {
            list.first = prvek;
            rightBox.previous = prvek;
        } else {
            leftBox.next = prvek;
            rightBox.previous = prvek;
        }
        prvek.previous = leftBox;
        prvek.next = rightBox;
        leftBox = prvek;
        index++;
    }
}
