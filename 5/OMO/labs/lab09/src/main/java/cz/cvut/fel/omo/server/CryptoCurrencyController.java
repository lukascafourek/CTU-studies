package cz.cvut.fel.omo.server;

import cz.cvut.fel.omo.Observable;
import cz.cvut.fel.omo.Observer;

import javax.money.MonetaryAmount;
import java.util.HashSet;
import java.util.Set;

public abstract class CryptoCurrencyController implements Observable {

    private final static int PERCENTAGE = 100;

    private final CryptoCurrency currency;

    private final Set<Observer> observers = new HashSet<>();

    public CryptoCurrencyController(CryptoCurrency currency) {
        this.currency = currency;
    }

    public abstract int computeFluctuation();

    /*
     * Method for calculating the new price of cryptocurrency.
     */
    public void changePrice() {
        MonetaryAmount currentPrice = currency.getPrice();
        MonetaryAmount change = currentPrice.multiply(this.computeFluctuation()).divide(PERCENTAGE);
        currency.setPrice(currentPrice.add(change));
        currency.printMessage();
        this.notifyAllObservers();
    }

    @Override
    public void attach(Observer observer) {
        this.observers.add(observer);
    }

    @Override
    public void detach(Observer observer) {
        this.observers.remove(observer);
    }

    @Override
    public void notifyAllObservers() {
        this.observers.forEach(observer -> observer.update(currency.getPrice()));
    }

    public MonetaryAmount getState() {
        return currency.getPrice();
    }

    public CryptoCurrency getCurrency() {
        return currency;
    }
}
