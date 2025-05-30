package cz.cvut.fel.pjv;

import java.util.concurrent.Callable;

public class MyCallable implements Callable<Integer> {
  private final int id;

  public MyCallable(int id) {
    this.id = id;
  }

  @Override
  public Integer call() throws Exception {
    for (int i = 1; i <= 10 ; i++) {
      System.out.printf("%s task ID: %d counter: %d%n", Thread.currentThread().getName(), id, i);
      try {
        Thread.sleep(50);
      } catch (InterruptedException e) {
        System.out.println("Interrupted");
        return 100 + id;
      }
    }
    return 100 + id;
  }
}
