package cz.cvut.fel.omo.cv10;

import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class TradeHistoryTest {
    private TradeHistory tradeHistory;

    @Before
    public void setUp() {
        Trader raoul = new Trader("Raoul", "Cambridge", false);
        Trader mario = new Trader("Mario", "Milano", true);
        Trader alan = new Trader("Alan", "Cambridge", false);
        Trader brian = new Trader("Brian", "Cambridge", false);
        Trader bohous = new Trader("Bohous", "Kladno", false);
        Trader jarous = new Trader("Jarous", "Kladno", true);

        List<Transaction> transactions = Arrays.asList(
                new Transaction(brian, 2011, 300),
                new Transaction(raoul, 2012, 1000),
                new Transaction(raoul, 2011, 400),
                new Transaction(mario, 2012, 20),
                new Transaction(mario, 2012, 10),
                new Transaction(alan, 2012, 950),
                new Transaction(bohous, 2012, 1000),
                new Transaction(bohous, 2011, 1100),
                new Transaction(bohous, 2012, 800),
                new Transaction(jarous, 2011, 10),
                new Transaction(jarous, 2011, 30));

        tradeHistory = new TradeHistory(transactions);

    }

    @Test
    public void findAllTransactionsIn2011AndSortByValueAsc() {
        //act
        List<Transaction> actual = tradeHistory.findAllTransactionsIn2011AndSortByValueAsc();

        //assert
        assertEquals(actual.get(0).getValue(), 10);
        assertEquals(actual.get(1).getValue(), 30);
    }

    @Test
    public void getUniqueCitiesSortedAsc() {
        //arrange
        String[] expected = new String[]{"Cambridge", "Kladno", "Milano"};

        //act
        List<String> test = tradeHistory.getUniqueCitiesSortedAsc();


        //assert
        assertTrue(Arrays.deepEquals(expected, test.toArray()));
    }

    @Test
    public void getSingleStringFromUniqueTradersNamesSortByNameAsc() {
        //arrange
        String expected = "Traders: Alan Bohous Brian Jarous Mario Raoul";

        //act
        String actual = tradeHistory.getSingleStringFromUniqueTradersNamesSortByNameAsc();

        //assert
        assertEquals(expected, actual);

    }

    @Test
    public void isSomeTraderFromCity() {

        //arrange
        String test1 = "Kladno";
        String test2 = "Brno";

        //act & assert
        assertTrue(tradeHistory.isSomeTraderFromCity(test1));
        assertFalse(tradeHistory.isSomeTraderFromCity(test2));
    }

    @Test
    public void findSmallestTransactionUsingReduce() {
        //act
        Optional<Transaction> transactionOpt = tradeHistory.findSmallestTransactionUsingReduce();

        //assert
        if (transactionOpt.isPresent())
            assertEquals(transactionOpt.get().getValue(), 10);
        else
            assertFalse(false);

    }

    @Test
    public void getTraderNamesByTown() {
        //arrange
        String[] kladnoTradersLabel = new String[]{"Bohous", "Jarous"};
        String[] cambridgeTradersLabel = new String[]{"Alan", "Brian", "Raoul"};
        String[] milanoTradersLabel = new String[]{"Mario"};

        //act
        Map<String, List<Trader>> traders = tradeHistory.getTradersByTown();

        List<String> kladnoTradersTest = traders.get("Kladno").stream().map(Trader::getName).sorted()
                                                .collect(Collectors.toList());
        List<String> cambridgeTradersTest = traders.get("Cambridge").stream().map(Trader::getName).sorted()
                                                   .collect(Collectors.toList());
        List<String> milanoTradersTest = traders.get("Milano").stream().map(Trader::getName).sorted()
                                                .collect(Collectors.toList());

        //assert
        assertArrayEquals(kladnoTradersLabel, kladnoTradersTest.toArray());
        assertArrayEquals(cambridgeTradersLabel, cambridgeTradersTest.toArray());
        assertArrayEquals(milanoTradersLabel, milanoTradersTest.toArray());
    }

    @Test
    public void getTradersCountsByTown() {

        //act
        Map<String, Long> traders = tradeHistory.getTradersCountsByTown();

        //assert
        assertEquals(2, traders.get("Kladno").intValue());
        assertEquals(3, traders.get("Cambridge").intValue());
        assertEquals(1, traders.get("Milano").intValue());

    }

    @Test
    public void partitionTransactionsByTraderIsVegetarian() {
        //act
        Map<Boolean, List<Transaction>> traders = tradeHistory.partitionTransactionsByTraderIsVegetarian();

        //assert - simplified test checking only size of both groups
        assertEquals(traders.get(true).size(), 4);
        assertEquals(traders.get(false).size(), 7);
    }
}