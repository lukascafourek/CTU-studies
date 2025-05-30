package cz.cvut.fel.pjv;

import java.util.Scanner;

public class DayOfWeek {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Zadejte číslo reprezentující den v týdnu (1-7):");
        int cisloDne = scanner.nextInt();

        String den = prevedCisloNaDen(cisloDne);
        System.out.println("Den v týdnu: " + den);

        scanner.close();
    }

    public static String prevedCisloNaDen(int cislo) {
        switch (cislo) {
            case 1:
                return "Monday";
            case 2:
                return "Tuesday";
            case 3:
                return "Wednesday";
            case 4:
                return "Thursday";
            case 5:
                return "Friday";
            case 6:
                return "Saturday";
            case 7:
                return "Sunday";
            default:
                return "Neplatné číslo dne";
        }
    }
}
