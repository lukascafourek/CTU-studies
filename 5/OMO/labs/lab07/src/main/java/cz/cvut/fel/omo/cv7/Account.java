package cz.cvut.fel.omo.cv7;

import javax.money.MonetaryAmount;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 7 Abstract factory, factory method, singleton, dependency injection
 *
 * @author mayerto1
 */
public interface Account {
    MonetaryAmount getBalance();

    MonetaryAmount getWithdrawLimit();

    MonetaryAmount getMonthlyFee();

    void withdraw(MonetaryAmount amount);

    void deposit(MonetaryAmount amount);
}
