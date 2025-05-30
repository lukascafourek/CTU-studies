package cz.cvut.fel.omo;

import javax.money.MonetaryAmount;

public interface Observer {

    void update(MonetaryAmount currentValue);
}
