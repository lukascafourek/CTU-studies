package cz.cvut.fel.pjv;

import java.util.Locale;
import java.util.Scanner;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class Lab01 {

    static void getFirstOperand(int i) {
        switch (i) {
            case 1 : System.out.println("Zadej scitanec: ");
                     break;
            case 2 : System.out.println("Zadej mensenec: ");
                     break;
            case 3 : System.out.println("Zadej cinitel: ");
                     break;
            case 4 : System.out.println("Zadej delenec: ");
                     break;
        }
    }
    static void getSecondOperand(int i) {
        switch (i) {
            case 1 : System.out.println("Zadej scitanec: ");
                     break;
            case 2 : System.out.println("Zadej mensitel: ");
                     break;
            case 3 : System.out.println("Zadej cinitel: ");
                     break;
            case 4 : System.out.println("Zadej delitel: ");
                     break;
        }
    }
    static void calculate(int i, double d1, double d2, int m) {
        switch (i) {
            case 1 : System.out.printf("%." + m + "f" + " + " + "%." + m + "f" +
                                       " = " + "%." + m + "f" + "\n", d1, d2, d1 + d2);
                     break;
            case 2 : System.out.printf("%." + m + "f" + " - " + "%." + m + "f" +
                                       " = " + "%." + m + "f" + "\n", d1, d2, d1 - d2);
                     break;
            case 3 : System.out.printf("%." + m + "f" + " * " + "%." + m + "f" +
                                       " = " + "%." + m + "f" + "\n", d1, d2, d1 * d2);
                     break;
            case 4 : System.out.printf("%." + m + "f" + " / " + "%." + m + "f" +
                                       " = " + "%." + m + "f" + "\n", d1, d2, d1 / d2);
                     break;
        }
    }
    public void homework() {
        System.out.println("Vyber operaci (1-soucet, 2-rozdil, 3-soucin, 4-podil):");
        Scanner sc = new Scanner(System.in);
        sc.useLocale(Locale.US);
        int i = sc.nextInt();
        if (i != 1 && i != 2 && i != 3 && i != 4) {
            System.out.println("Chybna volba!");
            return;
        }
        getFirstOperand(i);
        double d1 = sc.nextDouble();
        getSecondOperand(i);
        double d2 = sc.nextDouble();
        if (i == 4 && d2 == 0) {
            System.out.println("Pokus o deleni nulou!");
            return;
        }
        System.out.println("Zadej pocet desetinnych mist: ");
        int m = sc.nextInt();
        if (m < 0) {
            System.out.println("Chyba - musi byt zadane kladne cislo!");
            return;
        }
        calculate(i, d1, d2, m);
    }

}
