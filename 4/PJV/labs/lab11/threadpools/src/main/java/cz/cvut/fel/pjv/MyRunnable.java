package cz.cvut.fel.pjv;

public class MyRunnable implements Runnable {
  private final int id;

  public MyRunnable(int id) {
    this.id = id;
  }

  @Override
  public void run() {
    for (int i = 1; i <= 10 ; i++) {
      System.out.printf("%s task ID: %d counter: %d%n", Thread.currentThread().getName(), id, i);
      try {
        Thread.sleep(50);
      } catch (InterruptedException e) {
        System.out.println("Interrupted");
        return;
      }
    }
  }
}
