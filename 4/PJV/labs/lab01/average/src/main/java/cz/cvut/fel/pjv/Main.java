package cz.cvut.fel.pjv;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        float sum = 0;
        int count = 0;
//        int count = args.length;;
//        for (String arg : args) {
//            sum += Integer.valueOf(arg);
//        }
        Scanner sc = new Scanner(System.in);
        while (sc.hasNextInt()) {
            sum += sc.nextInt();
            count++;
        }
        System.out.println(sum / count);
    }
}
