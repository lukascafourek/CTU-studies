package cz.cvut.fel.ts1.archive;

import cz.cvut.fel.ts1.shop.Item;
import cz.cvut.fel.ts1.shop.Order;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class PurchasesArchiveTest {

    @Test
    public void testPrintItemPurchaseStatistics() {
        PurchasesArchive archive = new PurchasesArchive();
        HashMap<Integer, ItemPurchaseArchiveEntry> mockItemArchive = new HashMap<>();
        mockItemArchive.put(1, new ItemPurchaseArchiveEntry(new Item(1, "Item1", 10.0f, "Category1")));
        mockItemArchive.put(2, new ItemPurchaseArchiveEntry(new Item(2, "Item2", 20.0f, "Category2")));
        archive.itemPurchaseArchive = mockItemArchive;

        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));

        archive.printItemPurchaseStatistics();

        System.setOut(System.out);

        String expectedOutput = "ITEM PURCHASE STATISTICS:" + System.lineSeparator() +
                "ITEM  Item   ID 1   NAME Item1   CATEGORY Category1   HAS BEEN SOLD 1 TIMES" + System.lineSeparator() +
                "ITEM  Item   ID 2   NAME Item2   CATEGORY Category2   HAS BEEN SOLD 1 TIMES" + System.lineSeparator();
        assertEquals(expectedOutput, outContent.toString());
    }

    @Test
    public void testGetHowManyTimesHasBeenItemSold_ItemExists() {
        PurchasesArchive archive = new PurchasesArchive();
        ItemPurchaseArchiveEntry mockEntry = Mockito.mock(ItemPurchaseArchiveEntry.class);
        when(mockEntry.getCountHowManyTimesHasBeenSold()).thenReturn(10);
        archive.itemPurchaseArchive.put(1, mockEntry);

        assertEquals(10, archive.getHowManyTimesHasBeenItemSold(new Item(1, "TestItem", 10.0f, "TestCategory")));
    }

    @Test
    public void testGetHowManyTimesHasBeenItemSold_ItemDoesNotExist() {
        PurchasesArchive archive = new PurchasesArchive();
        assertEquals(0, archive.getHowManyTimesHasBeenItemSold(new Item(1, "NonExistentItem", 10.0f, "TestCategory")));
    }

    @Test
    public void testPutOrderToPurchasesArchive() {
        PurchasesArchive archive = new PurchasesArchive();
        Order mockOrder = Mockito.mock(Order.class);
        Item item1 = new Item(1, "Item1", 10.0f, "Category1");
        Item item2 = new Item(2, "Item2", 20.0f, "Category2");
        when(mockOrder.getItems()).thenReturn(new ArrayList<>(List.of(item1, item2)));

        archive.putOrderToPurchasesArchive(mockOrder);

        assertTrue(archive.orderArchive.contains(mockOrder));

        assertEquals(1, archive.itemPurchaseArchive.get(1).getCountHowManyTimesHasBeenSold());
        assertEquals(1, archive.itemPurchaseArchive.get(2).getCountHowManyTimesHasBeenSold());
    }

    @Test
    public void testPrintlnMethodInPrintItemPurchaseStatistics() {
        PurchasesArchive archive = new PurchasesArchive();
        HashMap<Integer, ItemPurchaseArchiveEntry> mockItemArchive = new HashMap<>();
        mockItemArchive.put(1, new ItemPurchaseArchiveEntry(new Item(1, "Item1", 10.0f, "Category1")));
        archive.itemPurchaseArchive = mockItemArchive;

        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));

        archive.printItemPurchaseStatistics();

        System.setOut(System.out);

        assertTrue(outContent.toString().contains("ITEM PURCHASE STATISTICS:"));
    }

    @Test
    public void testMockOrderArchiveAndItemPurchaseArchiveEntryConstructor() {
        HashMap mockItemArchive = mock(HashMap.class);
        ArrayList mockOrderArchive = mock(ArrayList.class);

        PurchasesArchive purchasesArchive = new PurchasesArchive(mockItemArchive, mockOrderArchive);

        assertSame(mockItemArchive, purchasesArchive.itemPurchaseArchive);
        assertSame(mockOrderArchive, purchasesArchive.orderArchive);
    }
}
