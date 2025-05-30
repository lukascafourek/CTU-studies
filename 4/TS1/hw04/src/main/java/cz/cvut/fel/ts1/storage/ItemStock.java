package cz.cvut.fel.ts1.storage;

import cz.cvut.fel.ts1.shop.Item;


/**
 * Auxiliary class for item storage
 */
public class ItemStock {
    private Item refItem;
    private int count;
    
    ItemStock(Item refItem) {
        this.refItem = refItem;
        count = 0;
    }
    
    @Override
    public String toString() {
        return "STOCK OF ITEM:  "+refItem.toString()+"    PIECES IN STORAGE: "+count;
    }
    
    void IncreaseItemCount(int x) {
        count += x;
    }
    
    void decreaseItemCount(int x) {
        count -= x;
        if (count < 0) {
            count = 0;
        }
    }
    
    int getCount() {
        return count;
    }
    
    Item getItem() {
        return refItem;
    }
}
