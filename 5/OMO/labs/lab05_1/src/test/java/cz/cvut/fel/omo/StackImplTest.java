package cz.cvut.fel.omo;

import org.junit.Before;
import org.junit.Test;

import java.util.EmptyStackException;

import static org.junit.Assert.assertEquals;

public class StackImplTest {

    private Stack stack = new StackImpl();

    @Before
    public void init() {
        stack = new StackImpl();
    }

    @Test
    public void push_pushMultipleItems_stackSizeIncremented() {
        // arrange
        int toInsert = 1;
        int expectedSize = 3;
        // act
        for (int i = 0; i < expectedSize; i++) {
            stack.push(toInsert);
        }
        int actualSize = stack.getSize();
        // assert
        assertEquals(expectedSize, actualSize);
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