package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.Message;

/**
 * Zprava na prevod penez
 */
public class MoneyTransfer extends Message {
  public final int amount;

  public MoneyTransfer(int amount) {
    this.amount = amount;
  }
}
