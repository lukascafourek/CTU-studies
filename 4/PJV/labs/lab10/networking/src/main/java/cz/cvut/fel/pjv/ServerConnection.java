package cz.cvut.fel.pjv;

import java.io.*;
import java.net.Socket;

public class ServerConnection implements Runnable {
  private final Socket socket;

  public ServerConnection(Socket socket) {
    this.socket = socket;
  }

  @Override
  public void run() {
    try (
      BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      PrintWriter pw = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()),true);
    ) {
      String msg;
      while ((msg = br.readLine()) != null) {
        System.out.println("RECEIVED>" + msg);
        pw.println("ECHO:" + msg);
      }
    } catch (IOException ex) {
      System.out.println("Cant read/write " + ex);
    }
  }
}
