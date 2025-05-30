package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.DSConfig;
import cz.cvut.fel.agents.pdv.dsand.DSProcess;
import cz.cvut.fel.agents.pdv.dsand.IStateSnapshotCollector;
import cz.cvut.fel.agents.pdv.dsand.Message;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Konfigurace simulace
 */
public class SnapshotDSConfig implements DSConfig, IStateSnapshotCollector {

  private static final Random RANDOM = new Random();
  // pocet bank ve scenari, minimum jsou 3
  private final Set<String> banks;
  // pocet klientu ve scenari, minimum je 1
  private final Set<String> clients;

  // promenne pro kontrolu spravnosti
  private int amount = 0;
  private final List<Integer> balances = new ArrayList<>();

  public SnapshotDSConfig(int countOfBanks, int countOfClients) {
    System.out.println("Inicializuji konfiguraci se #" + Math.max(countOfBanks, 3) + " bankami a "
        + "#" + Math.max(countOfClients, 1) + " klienty.");
    this.banks = IntStream.range(0, Math.max(countOfBanks, 3))
        .boxed()
        .map(Object::toString)
        .collect(Collectors.toSet());
    this.clients = IntStream.range(banks.size(), banks.size() + Math.max(countOfClients, 1))
        .boxed()
        .map(Object::toString)
        .collect(Collectors.toSet());
  }

  @Override
  public String[] getProcessIDs() {
    return Stream.concat(banks.stream(), clients.stream())
        .toArray(String[]::new);
  }

  @Override
  public DSProcess instantiateProcess(String id, Queue<Message> inbox,
      BiConsumer<String, Message> outbox, String[] directory) {
    if (banks.contains(id)) {
      int account = getAmount();
      amount = amount + account;
      return new BankProcess(id, inbox, outbox, banks.size(), account, this);
    }
    return new ClientProcess(id, inbox, outbox, new ArrayList<>(banks));
  }

  /**
   * Vygeneruje pocatecni hodnotu stavu uctu
   */
  private static int getAmount() {
    return 10000 + (RANDOM.nextInt(10) + 1) * RANDOM.nextInt(1000);
  }

  @Override
  public long getDeliveryDelay(String src, String dst, boolean isRecordMessage) {
    if (isRecordMessage) {
      return RANDOM.nextInt(20) + 5;
    }
    return 1;
  }

  @Override
  public double getReliability(String src, String dst) {
    return 1.0;
  }

  @Override
  public List<String> getProcessesToWake() {
    return Arrays.asList(getProcessIDs());
  }

  @Override
  public boolean isRunning() {

    //mame vsechny stavy?
    if (balances.size() == banks.size()) {
      int sum = balances.stream()
          .mapToInt(value -> value)
          .sum();
      String balancesAsS = balances.stream()
          .map(integer -> integer + " Kc")
          .collect(Collectors.joining(", "));
      System.out.println("\nBilance podle bank: " + balancesAsS);
      if (amount == sum) {
        System.out.println("OK: Ocekavana hodnota " + amount + " Kc, zaznamenana " + sum + " Kc");
      } else {
        System.err.println("Chyba: Ocekavana hodnota " + amount +
            " Kc se lisi od zaznamenane " + sum + " Kc, pri korektni implementaci se v systemu "
            + "nemohou ztracet penize.");
      }
    }

    return balances.size() != banks.size();
  }

  @Override
  public void sendSnapshotOfBalance(int balance) {
    balances.add(balance);
  }
}
