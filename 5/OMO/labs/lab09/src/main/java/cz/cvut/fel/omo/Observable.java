package cz.cvut.fel.omo;

public interface Observable {

    void attach(Observer observer);

    void detach(Observer observer);

    void notifyAllObservers();
}
