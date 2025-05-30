package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.Message;

/**
 * Klientsky pozadavek na prevod penez
 */
public class ClientRequest extends Message {

  // do banky
  public final String to;
  // castka
  public final int amount;

  public ClientRequest(String to, int amount) {
    this.to = to;
    this.amount = amount;
  }
}
