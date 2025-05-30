package cz.cvut.fel.omo.client;

import cz.cvut.fel.omo.Observer;
import cz.cvut.fel.omo.server.CryptoCurrency;

import javax.money.MonetaryAmount;

public class BitcoinClient implements Observer {

    private CryptoCurrency currency;

    public BitcoinClient(CryptoCurrency currency) {
        this.currency = currency;
    }

    @Override
    public void update(MonetaryAmount currentValue) {
        System.out.println("Current value of " + currency.getCurrencyName() + " is: " + currentValue);
    }

    public CryptoCurrency getCurrency() {
        return currency;
    }
}
