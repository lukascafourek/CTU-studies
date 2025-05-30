package cz.cvut.fel.pjv;

import java.util.Arrays;

public class ArraySorter {
    public static void main(String[] args) {
        int[] pole = {5, 2, 7, 1, 3};
        sortArray(pole);
        printArray(pole);
    }

    public static void sortArray(int[] array) {
        Arrays.sort(array);
    }

    public static void printArray(int[] array) {
        System.out.print("Seřazené pole: ");
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i]);
            if (i < array.length - 1) {
                System.out.print(", ");
            }
        }
        System.out.println();
    }
}
