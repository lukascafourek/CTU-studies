package cz.cvut.fel.omo.server;

import java.util.Random;

public class LitecoinController extends CryptoCurrencyController {

    private final static int LITECOIN_RANGE = 15;
    private final static int LITECOIN_COEFFICIENT = 5;

    public LitecoinController() {
        super(new Litecoin());
    }

    @Override
    public int computeFluctuation() {
        Random rand = new Random();
        return rand.nextInt(LITECOIN_RANGE) - LITECOIN_COEFFICIENT;
    }
}
