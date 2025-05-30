package cz.cvut.fel.omo.homeworks.old;

public class Application {

    public static void main(String[] args) {
        TransactionSystem transactionSystem = new TransactionSystem();
        transactionSystem.setTransactionParams(1000L, "CZK");
        System.out.println(transactionSystem.executeTransaction());

        transactionSystem.setTransactionParams(1000L, "EUR", "http://fel.cvut.cz/sucess", "http://fel.cvut.cz/sad-smiley");
        System.out.println(transactionSystem.executeTransaction());
    }
}
