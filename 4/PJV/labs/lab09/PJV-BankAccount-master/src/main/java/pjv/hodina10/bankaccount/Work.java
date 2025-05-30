
package pjv.hodina10.bankaccount;

import java.util.List;

public class Work {    
    private List<Person> employees;
    private double [] payments; 
    
    public Work(List<Person> employees, double [] payments){
        this.employees = employees;
        this.payments = payments;
    }
    
    public void payPayments() throws InterruptedException {
        for (Person employee : employees) {
            for (double payment : payments) {
                employee.addAmountToBankAccount(payment);
            }
        }
    }
}
