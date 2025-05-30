package cz.cvut.fel.pjv;

import java.io.*;
import java.util.Scanner;

public class FileIo {
  public void replaceWhitespaces(String inFileName, String outFileName) {
    try (
      FileReader fr = new FileReader(inFileName);
      FileWriter fw = new FileWriter(outFileName);
    ) {
      int c;
      boolean isPreviousSpace = false;
      while ((c = fr.read()) != -1) {
        if (!Character.isWhitespace(c)) {
          fw.write(c);
          isPreviousSpace = false;
        }else{
          if(!isPreviousSpace){
            fw.write(' ');
            isPreviousSpace = true;
          }
        }
      }
    } catch (IOException e) {
      System.err.println("File IO problem: " + e.getMessage());
    }
  }

  public void numberLines(String inFileName, String outFileName) {
    try (
        BufferedReader br = new BufferedReader(new FileReader(inFileName));
        PrintWriter pw = new PrintWriter(new FileWriter(outFileName));
    ) {
      String line;
      int counter = 0;
      while ((line = br.readLine()) != null) {
        pw.print(counter);
        pw.println(line);
        counter++;
      }
    } catch (IOException e) {
      System.err.println("File IO problem: " + e.getMessage());
    }
  }

  public void countWords(String inFileName) {
    try (
        Scanner sc = new Scanner(new FileReader(inFileName));
    ) {
      int counter = 0;
      while (sc.hasNext()) {
        counter++;
        sc.next();
      }
      System.out.println(counter);
    } catch (IOException e) {
      System.err.println("File IO problem: " + e.getMessage());
    }
  }

  public void writeInt(String outFileName) {
    try (
        DataOutputStream dos = new DataOutputStream(new FileOutputStream(outFileName));
    ) {
      dos.writeInt(7);
      dos.writeInt(257);
    } catch (IOException e) {
      System.err.println("File IO problem: " + e.getMessage());
    }
  }

}
