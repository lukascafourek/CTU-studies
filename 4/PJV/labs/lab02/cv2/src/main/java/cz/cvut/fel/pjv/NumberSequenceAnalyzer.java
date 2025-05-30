package cz.cvut.fel.pjv;

import java.util.Scanner;

public class NumberSequenceAnalyzer {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int count = 0;
        int sum = 0;
        int max = Integer.MIN_VALUE;
        int secondMax = Integer.MIN_VALUE;
        int previous = Integer.MAX_VALUE;
        boolean constant = true;
        boolean increasing = true;
        boolean decreasing = true;

        System.out.println("Zadejte celá čísla. Pro ukončení zadejte 0:");

        while (true) {
            int number = scanner.nextInt();

            if (number == 0) {
                break;
            }

            sum += number;
            count++;

            if (number > max) {
                secondMax = max;
                max = number;
            } else if (number > secondMax && number != max) {
                secondMax = number;
            }

            if (number != previous) {
                constant = false;
            }
            if (number < previous) {
                increasing = false;
            }
            if (number > previous) {
                decreasing = false;
            }

            previous = number;
        }

        if (count == 0) {
            System.err.println("Unable to find average");
        } else {
            double average = (double) sum / count;
            System.out.println("Průměr: " + average);
        }

        if (secondMax == Integer.MIN_VALUE) {
            System.out.println("Druhý největší prvek není k dispozici.");
        } else {
            System.out.println("Druhý největší prvek: " + secondMax);
        }

        if (constant) {
            System.out.println("Posloupnost je konstantní.");
        } else if (increasing) {
            System.out.println("Posloupnost je rostoucí.");
        } else if (decreasing) {
            System.out.println("Posloupnost je klesající.");
        } else {
            System.out.println("Posloupnost je nerostoucí ani klesající.");
        }

        scanner.close();
    }
}
