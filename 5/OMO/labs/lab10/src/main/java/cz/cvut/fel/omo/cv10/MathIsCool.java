package cz.cvut.fel.omo.cv10;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class MathIsCool {

    public static List<double[]> generatePythagorianTriples(int from, int to) {
        return IntStream.rangeClosed(from, to)
                .boxed()
                .flatMap(a -> IntStream.rangeClosed(a, to)
                        .boxed()
                        .flatMap(b -> {
                            double c = Math.sqrt(a * a + b * b);
                            return (c == Math.floor(c))
                                    ? Stream.of(new double[]{a, b, c})
                                    : Stream.empty();
                        }))
                .collect(Collectors.toList());
    }

    public static List<int[]> generateFibonacciSeries(int from, int howMany) {
        List<int[]> fibonacciSeries = new ArrayList<>();
        fibonacciSeries.add(new int[]{0, 1});
        IntStream.iterate(1, i -> i + 1)
                .limit(howMany - 1)
                .forEach(i -> {
                    int[] previous = fibonacciSeries.get(fibonacciSeries.size() - 1);
                    fibonacciSeries.add(new int[]{previous[1], previous[0] + previous[1]});
                });
        return fibonacciSeries.stream()
                .skip(from)
                .limit(howMany)
                .collect(Collectors.toList());
    }
}
