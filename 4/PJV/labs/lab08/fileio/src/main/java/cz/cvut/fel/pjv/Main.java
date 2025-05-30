package cz.cvut.fel.pjv;

public class Main {
  public static void main(String[] args) {
    FileIo fileIo = new FileIo();
//    fileIo.replaceWhitespaces("input.txt", "output.txt");
//    fileIo.numberLines("input.txt", "output.txt");
//    fileIo.countWords("input.txt");
    fileIo.writeInt("out.bin");
  }
}