package cz.cvut.fel.omo.client;

import cz.cvut.fel.omo.server.CryptoCurrency;
import cz.cvut.fel.omo.server.CurrencyName;
import cz.cvut.fel.omo.server.StockExchangeServer;

import java.util.HashMap;
import java.util.Map;

public class StockExchangeClient {

    private final StockExchangeServer stockExchangeServer;
    private final String name;
    private BitcoinClient bitcoinClient;
    private LitecoinClient litecoinClient;

//    private final Map<CurrencyName, CurrencyClient> clients = new HashMap<>();

    public StockExchangeClient(StockExchangeServer stockExchange, String name) {
        this.stockExchangeServer = stockExchange;
        this.name = name;
    }

    private void subscribeToUpdates(CryptoCurrency currency) {
//        this.clients.putIfAbsent(currency.getCurrencyName(), new CurrencyClient(currency));
//        this.stockExchangeServer.subscribeUpdates(currency, clients.get(currency.getCurrencyName()));
        if (currency.getCurrencyName().equals(CurrencyName.BITCOIN)) {
            this.subscribeToBitcoinChannel();
        } else if (currency.getCurrencyName().equals(CurrencyName.LITECOIN)) {
            this.subscribeToLitecoinChannel();
        }
    }

    /*
     * Method makes sure, that this client application is subscribed to both litecoin and bitcoin channel.
     */
    public void subscribeToAllChannels() {
//        this.stockExchangeServer.getAllCurrencies().forEach(this::subscribeToUpdates);
        this.subscribeToBitcoinChannel();
        this.subscribeToLitecoinChannel();
    }


    public void subscribeToBitcoinChannel() {
        CryptoCurrency currency = this.stockExchangeServer.getAllCurrencies().stream()
                .filter(cryptoCurrency -> cryptoCurrency.getCurrencyName().equals(CurrencyName.BITCOIN))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Bitcoin currency not found"));
        if (bitcoinClient == null) {
            bitcoinClient = new BitcoinClient(currency);
        }
        this.stockExchangeServer.subscribeUpdates(currency, bitcoinClient);
//        this.stockExchangeServer.getAllCurrencies().stream()
//                .filter(currency -> currency.getCurrencyName().equals(CurrencyName.BITCOIN))
//                .forEach(cryptoCurrency -> {
//                    if (bitcoinClient == null) {
//                        bitcoinClient = new BitcoinClient(cryptoCurrency);
//                    }
//                    this.stockExchangeServer.subscribeUpdates(cryptoCurrency, bitcoinClient);
//                });
    }

    public void subscribeToLitecoinChannel() {
        CryptoCurrency currency = this.stockExchangeServer.getAllCurrencies().stream()
                .filter(cryptoCurrency -> cryptoCurrency.getCurrencyName().equals(CurrencyName.LITECOIN))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Litecoin currency not found"));
        if (litecoinClient == null) {
            litecoinClient = new LitecoinClient(currency);
        }
        this.stockExchangeServer.subscribeUpdates(currency, litecoinClient);
//        this.stockExchangeServer.getAllCurrencies().stream()
//                .filter(currency -> currency.getCurrencyName().equals(CurrencyName.LITECOIN))
//                .forEach(cryptoCurrency -> {
//                    if (litecoinClient == null) {
//                        litecoinClient = new LitecoinClient(cryptoCurrency);
//                    }
//                    this.stockExchangeServer.subscribeUpdates(cryptoCurrency, litecoinClient);
//                });
    }

    /*
     * Method unsubscribes application from bitcoin channel, hereafter no notifications about bitcoin price will be delivered.
     */
    public void unsubscribeFromBitcoinChannel() {
        stockExchangeServer.unsubscribeFromBitcoinChannel(this.bitcoinClient);
    }

    /*
     * Method unsubscribes application from litecoin channel, hereafter no notifications about litecoin price will be delivered.
     */
    public void unsubscribeFromLitecoinChannel() {
        stockExchangeServer.unsubscribeFromLitecoinChannel(this.litecoinClient);
    }

    /*
     * Method unsubscribes application from both bitcoin and litecoin channels, hereafter no notifications about bitcoin nor litecoin price will be delivered.
     */
    public void unsubscribeFromAllChannels() {
        this.unsubscribeFromBitcoinChannel();
        this.unsubscribeFromLitecoinChannel();
//        this.clients.values().forEach(client -> stockExchangeServer.unsubscribeUpdates(client.getCurrency(), client));
    }

    public String getName() {
        return name;
    }

    public StockExchangeServer getServer() {
        return stockExchangeServer;
    }
}
