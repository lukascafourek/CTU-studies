package cz.cvut.fel.pjv;

import java.util.Random;

public class RandomArrayFiller {
    public static void main(String[] args) {
        int[] pole = new int[5];
        fillArray(pole, 1, 10); // Zde je interval <1, 10>
        printArray(pole);
    }

    public static void fillArray(int[] array, int min, int max) {
        Random rand = new Random();
        for (int i = 0; i < array.length; i++) {
            array[i] = rand.nextInt(max - min + 1) + min;
        }
    }

    public static void printArray(int[] array) {
        System.out.print("Obsah pole: ");
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i]);
            if (i < array.length - 1) {
                System.out.print(", ");
            }
        }
        System.out.println();
    }
}
