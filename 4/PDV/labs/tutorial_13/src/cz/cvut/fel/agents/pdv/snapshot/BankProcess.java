package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.DSProcess;
import cz.cvut.fel.agents.pdv.dsand.IStateSnapshotCollector;
import cz.cvut.fel.agents.pdv.dsand.Message;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.function.BiConsumer;

/**
 * Implementace bankovniho procesu. Vasim ukolem je dopsat metody processRecordRequest() a
 * recordTransaction().
 */
public class BankProcess extends DSProcess {

  // v tomto kroku proces s ID := 0 posle zadost o zaznamenani stavu systemu
  private static int stepWhenToCaptureSnapshot = 40;

  private int wakingTime = 0;
  private final int myID;
  private final int NUM_PROCESSES;

  // zde mate zachyceny/zachytte stav uctu
  private int stateOfAccount = 0;
  // zachyceni stavu procesu - je proces v rezimu zaznam
  private boolean stateRecording = false;

  // aktualni bilance uctu v bance, z tohoto uctu se posilaji penize nebo na tento ucet
  // prichazi penize
  private int account;
  private final IStateSnapshotCollector stateSnapshotCollector;

  // TODO
  // abyste mohli naimplementovat algoritmus vysvetleny na cviceni, bude treba si vytvorit nekolik
  // promennych, ktere Vam umozni ridit proces nahravani a ulozit si nejen aktualni stav procesu, ale
  // i prijate penize od ostatnich bank v dobe nahravani */

  public BankProcess(String id, Queue<Message> inbox, BiConsumer<String, Message> outbox,
      int numProcesses, int account, IStateSnapshotCollector stateSnapshotCollector) {
    super(id, inbox, outbox);
    this.NUM_PROCESSES = numProcesses;
    this.myID = Integer.parseInt(id);
    this.account = account;
    this.stateSnapshotCollector = stateSnapshotCollector;

    // TODO
  }

  @Override
  public void act() {
    wakingTime++;

    if (myID == 0 && wakingTime == stepWhenToCaptureSnapshot) {
	  // TODO
      // proces s id := 0 vyvola zadost o zaznamenani globalniho stavu a zaznamena si stav uctu
    }

    // zpracujte zpravy typu RecordMessage, tak ze doimplementujete metodu processRecordRequest()
    getRecordMessage()
        .forEach(recordMessage -> processRecordRequest(Integer.parseInt(recordMessage.sender)));

    // hlavni smycka na zpracovani zbytku zprav - prichazeji zpravy od klientu na bankovni transakce
    // (ktere jsou nasledne provedeny - vygeneruje se zprava na jinou banku) a zpravy
    // od ostatnich bank - na pricteni penez
    while (!inbox.isEmpty()) {
      Message m = inbox.poll();
      if (m instanceof MoneyTransfer) {

        // jina banka nam poslala penize
        int sender = Integer.parseInt(m.sender);
        int transferred = processMoneyTransfer((MoneyTransfer) m);
        recordTransaction(transferred, sender);
      } else {

        // klient nam poslal pozadavek, banka vygeneruje transakci
        processClientRequest((ClientRequest) m);
      }
    }
  }

  /**
   * Metoda provede klientuv pozadavek - prevod penez
   */
  private void processClientRequest(ClientRequest clientRequest) {

    int amountToSend = Math.min(clientRequest.amount, account);
    account = account - amountToSend;
    send(clientRequest.to, new MoneyTransfer(amountToSend));
    System.out.println(id + ": obdrzena zadost o prevod penez " + amountToSend + " Kc do "
        + clientRequest.to + ". banky");
  }

  /**
   * Metoda prijme penize z jine banky - a vrati cislo, o ktere se zmenila hodnota uctu (prijatou
   * castku)
   */
  private int processMoneyTransfer(MoneyTransfer moneyTransfer) {
    account = account + moneyTransfer.amount;
    System.out.println(id + ": obdrzeno " + moneyTransfer.amount + " Kc z " + moneyTransfer.sender +
        ". banky.");
    return moneyTransfer.amount;
  }

  /**
   * Metoda vrati seznam RecordMessage aktualne v inboxu
   */
  private List<RecordMessage> getRecordMessage() {

    // projdeme cely inbox a vybereme vsechny zpravy typu RecordMessage. ostatni vratime
    // zpet do fronty
    int size = inbox.size();
    List<RecordMessage> recordMessages = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      Message m = inbox.poll();
      if (m instanceof RecordMessage) {
        recordMessages.add((RecordMessage) m);
      } else {
        inbox.add(m);
      }
    }
    return recordMessages;
  }

  /**
   * Metoda pro osetreni pripadu, kdy je prijata RecordMessage od nejakeho odesilatele.
   */
  private void processRecordRequest(int sender) {

    //TODO
    // vasim ukolem je osetrit pripady, kdy doslo k prijeti zpravy typu RecordMessage od
    // odesilatele, ktery je v parametru metody. V ramci teto metody byste meli vyresit
    // 2 pripady - obdrzite zadost o zaznamenani a jeste nic nezaznamenavate, a pripad, kdy uz
    // zaznamenavate
    //
    // implementaci provedte na zaklade algoritmu probiraneho na cviceni. Jako soucasny stav procesu
    // pouzijte aktualni hodnotu uctu "account"
    //
    // v prvnim pripade se bude hodit jiz pripravena metoda sendRecordingMessages(), ktera posle vsem
    // ostatnim bankam zadost o zaznam
    //
    // v druhem pripade je treba po ukonceni zaznamu jeste vyrozumet centralni entitu, ktera
    // sbira jednotlive stavy pro vytvoreni globalniho snapshotu pomoci
    // stateSnapshotCollector.sendSnapshotOfBalance(), kde jako parametr vlozite celkovy objem penez
    // po dokonceni nahravani - penize v dobe, kdy jste zacali nahravat a penize z doslych transakci
    // v dobe nahravani

  }

  /**
   * Metoda na zaznamenani udalosti prijeti penezni castky od jine banky, abyste spravne mohli urcit
   * mnozstvi penez, ktere koluji v systemu
   */
  private void recordTransaction(int amount, int sender) {

    // TODO
    // vasim ukolem je rozhodnout, kdy zaznamenat prijeti penez od jine banky. V teto uloze mate
    // vyrazne zjednodusene zaznamenavani kanalu. Nemusite si ukladat zpravy, ale staci si jen drzet
    // informaci o mnozstvi penez, ktere zaslala jina banka v dobe, kdy jste zaznamenavali
    // transakce na kanale mezi touto bankou a jinou bankou

  }

  /**
   * Metoda odesle zpravu typu RecordMessage vsem ostatnim procesum
   */
  private void sendRecordingMessages() {
    for (int i = 0; i < NUM_PROCESSES; i++) {
      if (i != myID) {
        send(Integer.toString(i), new RecordMessage());
      }
    }
  }

}
