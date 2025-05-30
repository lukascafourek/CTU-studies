package cz.cvut.fel.pjv;

public class MyRunnable implements Runnable{
  private int counter = 20;
  @Override
  public void run() {
    for (int i = 10; i > 0; i--) {
      synchronized (this) {
        System.out.printf("%s i: %d count: %d%n", Thread.currentThread().getName(), i, counter);
        counter--;
      }
      try {
        Thread.sleep(50);
      } catch (InterruptedException e) {
        System.out.println("Interrupted");
      }
    }
  }
}
