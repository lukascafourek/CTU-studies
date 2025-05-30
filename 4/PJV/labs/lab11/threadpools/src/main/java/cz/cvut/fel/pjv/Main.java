package cz.cvut.fel.pjv;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class Main {
  public static void main(String[] args) throws ExecutionException, InterruptedException {
    ExecutorService threadPool = Executors.newFixedThreadPool(5);

    List<Future> futures = new ArrayList<>();
    for (int i = 0; i < 7; i++) {
      futures.add(threadPool.submit(new MyCallable(i)));
    }
    threadPool.shutdown();
    for (Future f : futures) {
      System.out.println(f.get());
    }

    System.out.println("Finished");
  }
}