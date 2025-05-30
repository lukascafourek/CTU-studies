package cz.cvut.fel.pjv;

import java.text.DecimalFormat;

public class CircleCalculator {
    public static void main(String[] args) {
        double polomer = 5.0; // Zadejte poloměr kruhu

        double obvod = vypocetObvodu(polomer);
        double obsah = vypocetObsahu(polomer);

        DecimalFormat df = new DecimalFormat("#.###");
        System.out.println("Obvod kruhu: " + df.format(obvod));
        System.out.println("Obsah kruhu: " + df.format(obsah));
    }

    public static double vypocetObvodu(double polomer) {
        return 2 * Math.PI * polomer;
    }

    public static double vypocetObsahu(double polomer) {
        return Math.PI * polomer * polomer;
    }
}
