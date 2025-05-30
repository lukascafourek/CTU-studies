package cz.cvut.fel.pjv;

import java.util.Scanner;

public class Calculator {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Zadejte první číslo:");
        double cislo1 = scanner.nextDouble();
        System.out.println("Zadejte druhé číslo:");
        double cislo2 = scanner.nextDouble();

        vypisOperace("Součet", cislo1 + cislo2);
        vypisOperace("Rozdíl", cislo1 - cislo2);
        vypisOperace("Součin", cislo1 * cislo2);
        if (cislo2 != 0) {
            vypisOperace("Podíl", cislo1 / cislo2);
        } else {
            System.out.println("Nelze dělit nulou.");
        }

        scanner.close();
    }

    public static void vypisOperace(String operace, double vysledek) {
        System.out.println(operace + ": " + vysledek);
    }
}
