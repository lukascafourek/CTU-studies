package cz.cvut.fel.ts1.shop;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class OrderTest {

    @Test
    public void testConstructorWithState() {
        ShoppingCart cart = new ShoppingCart();
        cart.addItem(new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5));
        String customerName = "John Doe";
        String customerAddress = "123 Main St";
        int state = 1;

        Order order = new Order(cart, customerName, customerAddress, state);

        assertNotNull(order);
        assertEquals(1, order.getItems().size());
        assertEquals(customerName, order.getCustomerName());
        assertEquals(customerAddress, order.getCustomerAddress());
        assertEquals(state, order.getState());
    }

    @Test
    public void testConstructorWithoutState() {
        ShoppingCart cart = new ShoppingCart();
        cart.addItem(new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5));
        String customerName = "John Doe";
        String customerAddress = "123 Main St";

        Order order = new Order(cart, customerName, customerAddress);

        assertNotNull(order);
        assertEquals(1, order.getItems().size());
        assertEquals(customerName, order.getCustomerName());
        assertEquals(customerAddress, order.getCustomerAddress());
        assertEquals(0, order.getState());
    }

    @Test
    public void testConstructorWithNullCart() {
        String customerName = "John Doe";
        String customerAddress = "123 Main St";

        Order order = new Order(null, customerName, customerAddress);

        assertNotNull(order);
        assertTrue(order.getItems().isEmpty());
        assertEquals(customerName, order.getCustomerName());
        assertEquals(customerAddress, order.getCustomerAddress());
        assertEquals(0, order.getState());
    }

    @Test
    public void testConstructorWithNullCustomerName() {
        ShoppingCart cart = new ShoppingCart();
        cart.addItem(new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5));
        String customerAddress = "123 Main St";

        Order order = new Order(cart, null, customerAddress);

        assertNotNull(order);
        assertEquals(1, order.getItems().size());
        assertNull(order.getCustomerName());
        assertEquals(customerAddress, order.getCustomerAddress());
        assertEquals(0, order.getState());
    }

    @Test
    public void testConstructorWithNullCustomerAddress() {
        ShoppingCart cart = new ShoppingCart();
        cart.addItem(new StandardItem(1, "TestItem", 10.0f, "TestCategory", 5));
        String customerName = "John Doe";

        Order order = new Order(cart, customerName, null);

        assertNotNull(order);
        assertEquals(1, order.getItems().size());
        assertEquals(customerName, order.getCustomerName());
        assertNull(order.getCustomerAddress());
        assertEquals(0, order.getState());
    }
}
