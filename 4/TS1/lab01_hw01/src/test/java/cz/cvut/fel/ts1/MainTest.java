package cz.cvut.fel.ts1;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class MainTest {

    // factorial tests
    @Test
    public void testFactorial() {
        // Test case 1: n = 0
        assertEquals(1, Main.factorial(0));

        // Test case 2: n = 1
        assertEquals(1, Main.factorial(1));

        // Test case 3: n = 5
        assertEquals(120, Main.factorial(5));

        // Test case 4: n = 10
        assertEquals(3628800, Main.factorial(10));

        // Test case 5: n = 12
        assertEquals(479001600, Main.factorial(12));
    }
}
