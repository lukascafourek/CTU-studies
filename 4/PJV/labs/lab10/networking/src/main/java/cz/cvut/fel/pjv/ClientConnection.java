package cz.cvut.fel.pjv;

import java.io.*;
import java.net.Socket;

public class ClientConnection implements Runnable {
  private final String host;
  private final int port;

  public ClientConnection(String host, int port) {
    this.host = host;
    this.port = port;
  }

  @Override
  public void run() {
    try (
      Socket socket = new Socket(host, port);
      BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      PrintWriter pw = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()),true);
    ) {
      String[] MESSAGES = {
          "10001,Pepa Nos,Dejvice,11:45 22.4.2021,ENTRY",
          "10002,Jan Pop,Karlovo náměstí,12:10 22.4.2021,EXIT",
          "10001,Pepa Nos,Dejvice,12:31 22.4.2021,ENTRY",
          "10002,Jan Pop,Karlovo náměstí,13:05 22.4.2021,EXIT"
      };
    for (String str : MESSAGES){
      pw.println(str);
      System.out.println(br.readLine());
    }


    } catch (IOException ex) {
      System.out.println("Cant read/write " + ex);
    }
  }
}
