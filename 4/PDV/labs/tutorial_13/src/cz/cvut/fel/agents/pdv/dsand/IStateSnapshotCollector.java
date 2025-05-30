package cz.cvut.fel.agents.pdv.dsand;

/**
 * Rozhrani pro tridu zpracovavajici nahrane stavy jednotlivych procesu
 */
public interface IStateSnapshotCollector {

  /**
   * Pres tuto metodu se posle centralni entite aktualni bilance uctu
   */
  void sendSnapshotOfBalance(int balance);

}
