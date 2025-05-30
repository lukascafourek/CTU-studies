package cz.cvut.fel.omo.cv10;

import java.util.*;
import java.util.stream.Collectors;

public class TradeHistory {

    public List<Transaction> transactions;

    public TradeHistory(List<Transaction> transctions) {
        this.transactions = transctions;
    }

    public List<Transaction> findAllTransactionsIn2011AndSortByValueAsc() {
        return transactions.stream()
            .filter(t -> t.getYear() == 2011)
            .sorted(Comparator.comparingInt(Transaction::getValue))
            .collect(Collectors.toList());
    }

    public List<String> getUniqueCitiesSortedAsc() {
        return transactions.stream()
                .map(t -> t.getTrader().getCity())
                .distinct()
                .sorted()
                .collect(Collectors.toList());
    }

    /*
     * String shall start with "Traders:" and use space as separator. E.g.: "Traders: Bob George"
     *
     */
    public String getSingleStringFromUniqueTradersNamesSortByNameAsc() {
        return transactions.stream()
            .map(t -> t.getTrader().getName())
            .distinct()
            .sorted()
            .reduce("Traders:", (s1, s2) -> s1 + " " + s2);
    }

    public boolean isSomeTraderFromCity(String cityName) {
        return transactions.stream()
                .anyMatch(t -> t.getTrader().getCity().equals(cityName));
    }

    public Optional<Transaction> findSmallestTransactionUsingReduce() {
        return transactions.stream()
                .reduce((t1, t2) -> t1.getValue() < t2.getValue() ? t1 : t2);
    }

    public Map<String, List<Trader>> getTradersByTown() {
        return transactions.stream()
                .map(Transaction::getTrader)
                .distinct()
                .collect(Collectors.groupingBy(Trader::getCity));
    }

    public Map<String, Long> getTradersCountsByTown() {
        return transactions.stream()
                .map(Transaction::getTrader)
                .distinct()
                .collect(Collectors.groupingBy(Trader::getCity, Collectors.counting()));
    }

    public Map<Boolean, List<Transaction>> partitionTransactionsByTraderIsVegetarian() {
        return transactions.stream()
                .collect(Collectors.partitioningBy(t -> t.getTrader().isVegetarian()));
    }
}
