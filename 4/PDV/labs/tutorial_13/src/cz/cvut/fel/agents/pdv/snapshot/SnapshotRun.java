package cz.cvut.fel.agents.pdv.snapshot;

import cz.cvut.fel.agents.pdv.dsand.Simulation;

/**
 * Trida s main metodou pro spusteni simulace
 */
public class SnapshotRun {

  public static void main(String[] args) {

    // V teto uloze budete implementovat algoritmus na zachyceni globalniho stavu. Vasim ukolem
    // bude implementovat Chandy-Lamport algoritmus pro zachyceni pohybu penez mezi nekolika
    // banakmi, ktere si prevadeji penize na zaklade pozadavku klientu. Ulohu jsme Vam v tomto
    // pripade mirne zjednodusili, kdy nemusite zaznamenavat zpravy, ale pouze castky, ktere si
    // banky v dobe zaznamu globalniho stavu prevadeji. Evaluace spravnosti implementace je v tomto
    // pripade velice jednoducha - pokud je vase implementace spravna, globalni stav penez
    // (dany stavem uctu a prijatymi penezi) by mel byt rovny celkovemu objemu penez v systemu -
    // v nasem pripade by se v systemu nemely objevovat nove penize, ani se ztracet ty puvodni,
    // pouze dochazi k jejich pohybu
    //
    // TODO: Otevrete si tridu BankProcess, kde najdete dalsi pokyny pro implementaci

    // spustime simulaci s konfiguraci scenare. Pro jednodusi ladeni si muzete zmenit pocet
    // bank nebo klientu. Minimalni pocet bank jsou 3, klient musi byt aspon 1
    Simulation sim = new Simulation(new SnapshotDSConfig(3, 2));
    sim.run();
  }
}
