package cz.cvut.fel.omo.homeworks.refactor.transaction;

import java.util.Optional;

public abstract class AbstractTransactionSystem implements TransactionSystem {

    protected Long totalAmount;
    protected String currencyCode;

    public AbstractTransactionSystem(Long totalAmount, String currencyCode) {
        this.totalAmount = totalAmount;
        this.currencyCode = currencyCode;
    }

    @Override
    public String executeTransaction() {
        Optional<String> transaction = buildTransaction();
        return transaction.map(s -> execute(s).orElse("failed")).orElse("Transaction execution failed.");
    }

    protected abstract Optional<String> execute(String transaction);

    protected abstract Optional<String> buildTransaction();
}
