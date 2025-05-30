package cz.cvut.fel.pjv;

import java.util.Scanner;

public class ReverseFactorial {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Select a non-negative number:");
        int number = sc.nextInt();
        int result = reverseFactorial(number);
        if (result == -1) {
            System.out.println(number + " does not have a factorial.");
        } else {
            System.out.println(number + " is " + result + "!");
        }
        sc.close();
    }

    public static int reverseFactorial(int number) {
        int divisor = 2;
        while (number > 1) {
            if (number % divisor != 0) {
                return -1; // Not a factorial
            }
            number /= divisor;
            divisor++;
        }
        return (number == 1) ? (divisor - 1) : -1;
    }
}
