package cz.cvut.fel.pjv;

public class Client {

  public static void main(String[] args) {
    new Thread(new ClientConnection("localhost", Server.PORT)).start();
  }
}
