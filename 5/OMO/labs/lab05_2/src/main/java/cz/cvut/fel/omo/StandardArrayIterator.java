package cz.cvut.fel.omo;

import java.util.NoSuchElementException;

/**
 * TODO: implement class
 */
public class StandardArrayIterator implements Iterator {

    private final int[] array;

    private int currentIndex = 0;

    public StandardArrayIterator(int[] array) {
        this.array = array;
    }

    @Override
    public int currentItem() {
        if (array.length == 0)
            throw new NoSuchElementException();
        return array[currentIndex];
    }

    @Override
    public int next() {
        if (currentIndex == array.length - 1)
            throw new NoSuchElementException();
        return array[++currentIndex];
    }

    @Override
    public boolean isDone() {
        return currentIndex == array.length - 1;
    }

    @Override
    public int first() {
        currentIndex = 0;
        return array[currentIndex];
    }
}
