package cz.cvut.fel.pjv;

public class ArrayFiller {
    public static void main(String[] args) {
        int[] pole = new int[5];
        fillArray(pole);
        printArray(pole);
    }

    public static void fillArray(int[] array) {
        for (int i = 0; i < array.length; i++) {
            array[i] = i + 1;
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
