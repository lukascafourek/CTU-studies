
package pjv.hodina10.bankaccount;

import java.util.List;


public class Shop {
    List<Person> customers;
    double [] bills;

    public Shop(List<Person> customers, double [] bills){
        this.customers = customers;
        this.bills = bills;
    }
    
    public void makeCustomersPayBills() throws InterruptedException{
         for(Person customer : customers){
            for(double payment : bills){
                customer.removeAmountFromBankAccount(payment);
            }
        }
    }
    
}
