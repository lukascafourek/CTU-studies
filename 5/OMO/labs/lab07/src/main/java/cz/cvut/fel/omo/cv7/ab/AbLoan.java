package cz.cvut.fel.omo.cv7.ab;

import cz.cvut.fel.omo.cv7.Loan;
import org.javamoney.moneta.Money;

import javax.money.MonetaryAmount;

public class AbLoan implements Loan {

    private MonetaryAmount balance;
    private final double interestRate;
    private final int repaymentPeriod;
    private final MonetaryAmount FIXED_FEE = Money.of(10, "EUR");

    public AbLoan(MonetaryAmount amount, int months, double recommendedInterestRate) {
        balance = amount;
        interestRate = recommendedInterestRate;
        repaymentPeriod = months;
    }

    @Override
    public MonetaryAmount getBalance() {
        return balance;
    }

    @Override
    public double getInterestRate() {
        return interestRate;
    }

    @Override
    public MonetaryAmount getMonthlyPayment() {
        return balance = balance.divide(repaymentPeriod)
                .add(balance.multiply(interestRate / 12)).add(FIXED_FEE);
    }

    public String toString() {
        return String.format("Loan Overview - Balance: %s, InterestRate: %f, MonthlyPayment: %s", getBalance(), getInterestRate(), getMonthlyPayment());
    }
}
