package cz.cvut.fel.pjv;

public class PrimeFactorization {
    public static void main(String[] args) {
        int number = 60;
        System.out.println(primeFactorizationText(number));
    }

    public static String primeFactorizationText(int number) {
        if (number < 2) {
            return "Prvočíselný rozklad není definován pro čísla menší než 2.";
        }

        StringBuilder result = new StringBuilder();
//        StringJoiner r = new StringJoiner("*", number + " = ", "");
        result.append(number).append(" = ");

        for (int i = 2; i <= number; i++) {
            while (number % i == 0) {
                result.append(i);
//                r.add(Integer.toString(i));
                number /= i;
                if (number != 1) {
                    result.append("*");
                }
            }
        }

        return result.toString();
//        return r.toString();
    }
}
