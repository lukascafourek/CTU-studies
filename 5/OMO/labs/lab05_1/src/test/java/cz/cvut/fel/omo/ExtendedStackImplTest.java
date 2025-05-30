package cz.cvut.fel.omo;

import org.junit.Before;
import org.junit.Test;

import java.util.EmptyStackException;
import java.util.NoSuchElementException;

import static org.junit.Assert.assertEquals;

public class ExtendedStackImplTest {

    private ExtendedStack stack = new ExtendedStackImpl();

    @Before
    public void init() {
        stack = new ExtendedStackImpl();
    }

    @Test
    public void push_pushItem_stackSizeIncremented() {
        // arrange
        int toInsert = 1;
        int expectedSize = stack.getSize() + 1;
        // act
        stack.push(toInsert);
        int actualSize = stack.getSize();
        // assert
        assertEquals(expectedSize, actualSize);
    }

    @Test
    public void push_pushArrayOfElements_stackSizeIncremented() {
        // arrange
        int[] toInsert = {5, 5, 7, 8, -1, 10, 15, 0, -5};
        int expectedValue = toInsert.length;
        // act
        stack.push(toInsert);
        // assert
        assertEquals(expectedValue, stack.getSize());
    }

    @Test
    public void top_stackIsNotEmpty_valueFromTopReceived() {
        // arrange
        int expectedValue = 4;
        stack.push(4);
        // act
        int actualValue = stack.top();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void top_stackIsNotEmpty_sizeOfStackUnchanged() {
        // arrange
        stack.push(4);
        int expectedValue = stack.getSize();
        // act
        stack.top();
        // assert
        assertEquals(expectedValue, stack.getSize());
    }

    @Test(expected = EmptyStackException.class)
    public void top_stackIsEmpty_exceptionThrown() {
        stack.top();
    }

    @Test
    public void pop_popFromNotEmptyStack_expectedValuePops() {
        // arrange
        int expectedValue = 55;
        stack.push(expectedValue);
        // act
        int actualValue = stack.pop();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test(expected = EmptyStackException.class)
    public void pop_popFromEmptyStack_emptyStackExceptionThrown() {
        // act
        stack.pop();
    }

    @Test(expected = EmptyStackException.class)
    public void popFirstNegativeElement_popFromEmptyStack_emptyStackExceptionThrown() {
        // act
        stack.popFirstNegativeElement();
    }

    @Test(expected = NoSuchElementException.class)
    public void popFirstNegativeElement_noNegativeElementInStack_noSuchElementExceptionThrown() {
        // arrange
        int[] toInsert = {4, 5, 2, 48, 0};
        stack.push(toInsert);
        // act
        stack.popFirstNegativeElement();
    }

    @Test
    public void popFirstNegativeElement_negativeElementInStackPresent_correctValueRetrieved() {
        // arrange
        int[] toInsert = {4, 5, -4, 48, 0};
        stack.push(toInsert);
        int expectedValue = -4;
        // act
        int actualValue = stack.popFirstNegativeElement();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void popFirstNegativeElement_negativeElementInStackPresent_sizeOfStackDecreased() {
        // arrange
        int[] toInsert = {4, 5, -4, 48, 0};
        stack.push(toInsert);
        int expectedValue = stack.getSize() - 1;
        // act
        stack.popFirstNegativeElement();
        // assert
        assertEquals(expectedValue, stack.getSize());
    }

    @Test
    public void popFirstNegativeElement_negativeElementInStackPresent_stackIsValidAfterPop() {
        // arrange
        int[] toInsert = {4, 5, -4, 48, 0};
        int[] expectedArray = {4, 5, 48, 0};
        stack.push(toInsert);
        // act
        stack.popFirstNegativeElement();
        // assert
        assertEquals(expectedArray[3], stack.pop());
    }

    @Test
    public void isEmpty_testingEmptyStack_returnTrue() {
        // arrange
        boolean expectedValue = true;
        // act
        boolean actualValue = stack.isEmpty();
        // assert
        assertEquals(expectedValue, actualValue);
    }

    @Test
    public void isEmpty_testingNotEmptyStack_returnFalse() {
        // arrange
        boolean expectedValue = false;
        stack.push(5);
        // act
        boolean actualValue = stack.isEmpty();
        // assert
        assertEquals(expectedValue, actualValue);
    }
}