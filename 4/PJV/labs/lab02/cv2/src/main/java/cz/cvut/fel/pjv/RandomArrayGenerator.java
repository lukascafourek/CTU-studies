package cz.cvut.fel.pjv;

import java.util.Random;

public class RandomArrayGenerator {
    public static void main(String[] args) {
        int rows = 3;
        int columns = 4;
        int min = 1;
        int max = 10;

        int[][] dvourozmernyPole = generateRandomArray(rows, columns, min, max);
        printArray(dvourozmernyPole);
    }

    public static int[][] generateRandomArray(int rows, int columns, int min, int max) {
        int[][] array = new int[rows][columns];
        Random rand = new Random();

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                array[i][j] = rand.nextInt(max - min + 1) + min;
            }
        }

        return array;
    }

    public static void printArray(int[][] array) {
        System.out.println("Dvourozměrné pole:");

        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < array[i].length; j++) {
                System.out.print(array[i][j] + " ");
            }
            System.out.println();
        }
    }
}
