package cz.cvut.fel.ts1;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter a non-negative integer to compute its factorial: ");
        int n = scanner.nextInt();

        if (n < 0) {
            System.out.println("Factorial is not defined for negative numbers.");
        } else {
            int num = factorial(n);
            System.out.println("Factorial of " + n + " is: " + num);
        }

        scanner.close();
    }

    //A factorial function
    public static int factorial(int n) {
        if (n == 0 || n == 1) {
            return 1;
        }

        int num = 1;
        for (int i = 2; i <= n; i++) {
            num *= i;
        }

        return num;
    }
}
