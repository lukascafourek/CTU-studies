package cz.cvut.fel.omo;

import org.junit.Before;
import org.junit.Test;

import java.util.NoSuchElementException;

import static org.junit.Assert.assertEquals;

public class StandardArrayIteratorTest {

    StandardArrayIterator iterator;

    @Before
    public void runBeforeEachTest() {
        int[] array = {4, 5};
        iterator = new StandardArrayIterator(array);
    }

    @Test
    public void currentItem_notEmptyArray_firstValueRetrieved() {
        // arrange
        int expectedValue = 4;
        // act
        int actualValue = iterator.currentItem();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test(expected = NoSuchElementException.class)
    public void currentItem_emptyArray_noSuchElementExceptionThrown() {
        // arrange
        int[] array = {};
        iterator = new StandardArrayIterator(array);
        // act
        iterator.currentItem();
    }

    @Test
    public void next_notOnEndOfArray_returnedNextElement() {
        // arrange
        int expectedValue = 5;
        // act
        int actualValue = iterator.next();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test(expected = NoSuchElementException.class)
    public void next_OnEndOfArray_noSuchElementExceptionThrown() {
        // arrange
        iterator.next();
        // act
        iterator.next();
    }

    @Test
    public void isDone_notOnTheEndOfArray_returnFalse() {
        // arrange
        boolean expectedValue = false;
        // act
        boolean actualValue = iterator.isDone();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void isDone_onTheEndOfArray_returnTrue() {
        // arrange
        boolean expectedValue = true;
        iterator.next();
        // act
        boolean actualValue = iterator.isDone();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void first_calledFromEndOfArray_firstElementOfArrayRetrieved() {
        // arrange
        int expectedValue = 4;
        iterator.next();
        // act
        int actualValue = iterator.first();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void first_calledFromEndOfArray_iteratorPointsAtTheStartOfArray() {
        // arrange
        int expectedValue = 4;
        iterator.next();
        // act
        iterator.first();
        // assert
        assertEquals(expectedValue, iterator.currentItem());
    }

}