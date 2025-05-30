package cz.cvut.fel.pjv;

public class ArrayPrinter2 {
    public static void main(String[] args) {
        int[][] dvourozmernyPole = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
        printArray(dvourozmernyPole);
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
