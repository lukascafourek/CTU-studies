package cz.cvut.fel.ts1.storage;

import cz.cvut.fel.ts1.shop.Item;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;

public class ItemStockTest {

    @ParameterizedTest
    @CsvSource({
            "1, TestItem, 10.0, TestCategory, 5, 0",
            "2, AnotherItem, 20.0, AnotherCategory, 10, 0",
            "3, YetAnotherItem, 30.0, YetAnotherCategory, 15, 0"
    })
    public void testConstructor(int id, String name, float price, String category, int loyaltyPoints, int expectedCount) {
        Item item = new Item(id, name, price, category);
        ItemStock itemStock = new ItemStock(item);

        assertEquals(item, itemStock.getItem());
        assertEquals(expectedCount, itemStock.getCount());
    }

    @ParameterizedTest
    @CsvSource({
            "5, 3, 8",
            "10, 5, 15",
            "0, 5, 5"
    })
    public void testIncreaseItemCount(int initialCount, int increaseAmount, int expectedCount) {
        Item item = new Item(1, "TestItem", 10.0f, "TestCategory");
        ItemStock itemStock = new ItemStock(item);
        itemStock.IncreaseItemCount(initialCount);
        itemStock.IncreaseItemCount(increaseAmount);

        assertEquals(expectedCount, itemStock.getCount());
    }

    @ParameterizedTest
    @CsvSource({
            "5, 3, 2",
            "10, 5, 5",
            "5, 10, 0"
    })
    public void testDecreaseItemCount(int initialCount, int decreaseAmount, int expectedCount) {
        Item item = new Item(1, "TestItem", 10.0f, "TestCategory");
        ItemStock itemStock = new ItemStock(item);
        itemStock.IncreaseItemCount(initialCount);
        itemStock.decreaseItemCount(decreaseAmount);

        assertEquals(expectedCount, itemStock.getCount());
    }
}
