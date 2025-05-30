package cz.cvut.fel.pjv;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
  final static int PORT = 4455;
  public static void main(String[] args) {
    try (
      ServerSocket serverSocket = new ServerSocket(PORT);
    ) {
      while (true) {
        System.out.println("Waiting for connection...");
        Socket socket = serverSocket.accept();
        System.out.println("Client connected");
        new Thread(new ServerConnection(socket)).start();
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}