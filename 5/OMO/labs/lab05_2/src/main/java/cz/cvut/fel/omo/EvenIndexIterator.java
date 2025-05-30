package cz.cvut.fel.omo;

/**
 * TODO: implement class
 */
public class EvenIndexIterator implements Iterator {

    private final StandardArrayIterator iterator;

    public EvenIndexIterator(int[] array) {
        int length = array.length % 2 == 0 ? array.length/2 : array.length/2 + 1;
        int j = 0;
        int[] arr = new int[length];
        for (int i = 0; i < array.length; i += 2) {
            arr[j++] = array[i];
        }
        iterator = new StandardArrayIterator(arr);
    }

    @Override
    public int currentItem() {
        return iterator.currentItem();
    }

    @Override
    public int next() {
        return iterator.next();
    }

    @Override
    public boolean isDone() {
        return iterator.isDone();
    }

    @Override
    public int first() {
        return iterator.first();
    }
}
