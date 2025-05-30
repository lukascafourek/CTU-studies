package cz.cvut.fel.omo;

/**
 * TODO: implement class
 */
public class ReverseArrayIterator implements Iterator {

    private final StandardArrayIterator iterator;

    public ReverseArrayIterator(int[] array) {
        int j = 0;
        int[] arr = new int[array.length];
        for (int i = array.length - 1; i >= 0; i--) {
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
