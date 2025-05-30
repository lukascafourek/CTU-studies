package cz.cvut.fel.ts1.shop;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;

public class StandardItemTest {

    @Test
    public void testConstructor() {
        StandardItem item = new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5);
        assertEquals(1, item.getID());
        assertEquals("TestItem", item.getName());
        assertEquals(10.0f, item.getPrice());
        assertEquals("TestCategory", item.getCategory());
        assertEquals(5, item.getLoyaltyPoints());
    }

    @Test
    public void testCopy() {
        StandardItem originalItem = new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5);
        StandardItem copiedItem = originalItem.copy();
        assertEquals(originalItem, copiedItem);
        assertNotSame(originalItem, copiedItem);
    }

    @ParameterizedTest
    @CsvSource({
            "1, TestItem, 10.0, TestCategory, 5, true",
            "1, AnotherItem, 10.0, TestCategory, 5, false",
            "1, TestItem, 20.0, TestCategory, 5, false",
            "1, TestItem, 10.0, AnotherCategory, 5, false",
            "1, TestItem, 10.0, TestCategory, 10, false",
            "2, TestItem, 10.0, TestCategory, 5, false"
    })
    public void testEquals(int id1, String name1, float price1, String category1, int loyaltyPoints1, boolean expected) {
        StandardItem item1 = new StandardItem(id1, name1, price1, category1, loyaltyPoints1);
        StandardItem item2 = new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5);
        assertEquals(expected, item1.equals(item2));
    }
}
