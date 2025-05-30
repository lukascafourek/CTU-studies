package cz.cvut.fel.pjv;

public class Main {
  public static void main(String[] args) {

//    new Thread() {
//      @Override
//      public void run() {
//        for (int i = 10; i > 0; i--) {
//          System.out.printf("Count: %d%n", i);
//        }
//      }
//    }.start();
//
//    new Thread(new Runnable() {
//      @Override
//      public void run() {
//        for (int i = 10; i > 0; i--) {
//          System.out.printf("Count: %d%n", i);
//        }
//      }
//    }).start();

    MyRunnable myRunnable = new MyRunnable();
    new Thread(myRunnable, "Prvni").start();
    new Thread(myRunnable, "Druhy").start();

    System.out.println(Thread.currentThread().getName() + " Hotovo");
  }
}