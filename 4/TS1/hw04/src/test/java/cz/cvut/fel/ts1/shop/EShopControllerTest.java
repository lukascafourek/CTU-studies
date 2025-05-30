package cz.cvut.fel.ts1.shop;

import cz.cvut.fel.ts1.storage.NoItemInStorage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class EShopControllerTest {

    @BeforeEach
    public void setUp() {
        EShopController.startEShop();
    }

    @Test
    public void testSuccessfulPurchase() throws NoItemInStorage {
        ShoppingCart cart = EShopController.newCart();

        int[] itemCount = {10,10,4};

        Item[] storageItems = {
                new StandardItem(1, "Dancing Panda v.2", 5000, "GADGETS", 5),
                new StandardItem(2, "Dancing Panda v.3 with USB port", 6000, "GADGETS", 10),
                new StandardItem(3, "Screwdriver", 200, "TOOLS", 5),
        };

        for (int i = 0; i < storageItems.length; i++) {
            EShopController.storage.insertItems(storageItems[i], itemCount[i]);
        }

        cart.addItem(storageItems[0]);
        cart.addItem(storageItems[1]);
        cart.addItem(storageItems[1]);
        cart.addItem(storageItems[2]);

        EShopController.purchaseShoppingCart(cart, "Libuse Novakova", "Kosmonautu 25, Praha 8");

        assertEquals(1, EShopController.archive.getHowManyTimesHasBeenItemSold(storageItems[0]));
        assertEquals(2, EShopController.archive.getHowManyTimesHasBeenItemSold(storageItems[1]));
        assertEquals(1, EShopController.archive.getHowManyTimesHasBeenItemSold(storageItems[2]));
        assertEquals(9, EShopController.storage.getItemCount(1));
        assertEquals(8, EShopController.storage.getItemCount(2));
        assertEquals(3, EShopController.storage.getItemCount(3));
    }

    @Test
    public void testInvalidCartPurchase() {
        ShoppingCart cart = EShopController.newCart();

        int[] itemCount = {1};

        Item[] storageItems = {
                new StandardItem(1, "Dancing Panda v.2", 5000, "GADGETS", 5),
        };

        for (int i = 0; i < storageItems.length; i++) {
            EShopController.storage.insertItems(storageItems[i], itemCount[i]);
        }

        cart.addItem(storageItems[0]);

        assertEquals(1, cart.getItemsCount());

        cart.removeItem(1);

        assertEquals(0, cart.getItemsCount());

        cart.addItem(storageItems[0]);
        cart.addItem(storageItems[0]);

        assertEquals(2, cart.getItemsCount());
        assertEquals(1, EShopController.storage.getItemCount(1));

        NoItemInStorage e = assertThrows(NoItemInStorage.class, () -> EShopController.purchaseShoppingCart
                (cart, "Jarmila Novakova", "Spojovaci 23, Praha 3"));

        assertEquals("No item in storage", e.getMessage());
    }
}
