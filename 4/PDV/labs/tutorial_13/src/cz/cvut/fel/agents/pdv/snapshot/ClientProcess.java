package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.DSProcess;
import cz.cvut.fel.agents.pdv.dsand.Message;
import java.util.List;
import java.util.Queue;
import java.util.Random;
import java.util.function.BiConsumer;

/**
 * Proces predstavujici bankovniho klienta, klient zadne zpravy necte, pouze je odesila
 */
public class ClientProcess extends DSProcess {

  private final static Random RANDOM = new Random();
  private final List<String> banks;
  private int clock = 0;

  // doba trvani jednoho intervalu mezi posilanim zprav
  private int endOfInterval = RANDOM.nextInt(2) + 1;

  public ClientProcess(String id, Queue<Message> inbox, BiConsumer<String, Message> outbox,
      List<String> banks) {
    super(id, inbox, outbox);
    this.banks = banks;
  }

  @Override
  public void act() {
    clock++;

    // na konci intervalu posleme zpravu
    if (clock % endOfInterval == 0) {
      String from = banks.get(RANDOM.nextInt(banks.size()));
      String to = banks.get(RANDOM.nextInt(banks.size()));
      while (to.equals(from)) {
        to = banks.get(RANDOM.nextInt(banks.size()));
      }
      int amount = RANDOM.nextInt(1000);
      send(from, new ClientRequest(to, amount));
    }
  }
}
