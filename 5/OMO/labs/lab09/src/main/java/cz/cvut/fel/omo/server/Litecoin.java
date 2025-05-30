package cz.cvut.fel.omo.server;

import org.javamoney.moneta.Money;

public class Litecoin extends CryptoCurrency {

    public Litecoin() {
        currencyName = CurrencyName.LITECOIN;
        price = Money.of(200, "EUR");
    }
}