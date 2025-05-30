package cz.cvut.fel.omo.server;

import java.util.Random;

public class BitcoinController extends CryptoCurrencyController {

    private final static int BITCOIN_RANGE = 20;
    private final static int BITCOIN_COEFFICIENT = 8;

    public BitcoinController() {
        super(new Bitcoin());
    }

    @Override
    public int computeFluctuation() {
        Random rand = new Random();
        return rand.nextInt(BITCOIN_RANGE) - BITCOIN_COEFFICIENT;
    }
}
