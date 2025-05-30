package cz.cvut.fel.pjv.cars.utils;

public class ArrayUtil {

  public void sort(int[] array) {
    for (int i = array.length - 1; i >= 0; i--) {
      for (int j = 0; j < i; j++) {
        if (array[j] > array[j + 1]) {
          int tmp = array[j + 1];
          array[j + 1] = array[j];
          array[j] = tmp;
        }
      }
    }
  }
}