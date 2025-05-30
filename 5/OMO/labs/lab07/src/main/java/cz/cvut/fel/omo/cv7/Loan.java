package cz.cvut.fel.omo.cv7;

import javax.money.MonetaryAmount;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 7 Abstract factory, factory method, singleton, dependency injection
 *
 * @author mayerto1
 */
public interface Loan {
    MonetaryAmount getBalance();

    double getInterestRate();

    MonetaryAmount getMonthlyPayment();
}
