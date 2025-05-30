package cz.cvut.fel.omo.cv7.ab;

import cz.cvut.fel.omo.cv7.Account;
import org.javamoney.moneta.Money;

import javax.money.MonetaryAmount;

public class AbAccount implements Account {

    private MonetaryAmount balance = Money.of(0, "EUR");
    private int withdrawCount = 0;
    private int depositCount = 0;

    @Override
    public MonetaryAmount getBalance() {
        return balance;
    }

    @Override
    public MonetaryAmount getWithdrawLimit() {
        return Money.of(2000, "EUR");
    }

    @Override
    public MonetaryAmount getMonthlyFee() {
        double fee = 0.5 * withdrawCount - 0.25 * depositCount;
        return Money.of(fee, "EUR");
    }

    @Override
    public void withdraw(MonetaryAmount amount) {
        balance = balance.subtract(amount);
        withdrawCount++;
    }

    @Override
    public void deposit(MonetaryAmount amount) {
        balance = balance.add(amount);
        depositCount++;
    }

    public String toString() {
        return String.format("Ab Account - balance: %s, fee: %s", getBalance(), getMonthlyFee());
    }
}
