package cz.cvut.fel.pjv;

public class ArrayPrinter {
    public static void main(String[] args) {
        int[] pole = {1, 2, 3, 4, 5};
        printArray(pole);
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
