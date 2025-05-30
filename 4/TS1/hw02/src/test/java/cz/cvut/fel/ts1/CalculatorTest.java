package cz.cvut.fel.ts1;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class CalculatorTest {

    @Test
    public void add_TwoPositiveNumbers_Passed() {
        assertEquals(5.0, Calculator.add(2.0, 3.0));
    }

    @Test
    public void subtract_TwoPositiveNumbers_Passed() {
        assertEquals(-1.0, Calculator.subtract(2.0, 3.0));
    }

    @Test
    public void multiply_TwoPositiveNumbers_Passed() {
        assertEquals(6.0, Calculator.multiply(2.0, 3.0));
    }

    @Test
    public void divide_TwoNegativeNumbers_Passed() {
        assertEquals(2.0, Calculator.divide(-6.0, -3.0));
    }

    @Test
    public void divide_DivisionByZero_ExceptionThrown() {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> Calculator.divide(2.0, 0.0));
        String expectedMessage = "Error: Division by zero!";
        String actualMessage = exception.getMessage();
        assertEquals(expectedMessage, actualMessage);
    }
}
