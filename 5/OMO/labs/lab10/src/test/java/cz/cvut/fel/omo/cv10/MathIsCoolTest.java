package cz.cvut.fel.omo.cv10;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

public class MathIsCoolTest {
    @Test
    public void generatePythagorianTriples_Between1And22_CorrectSequence() {
        // arrange
        List<double[]> label = Arrays.asList(
                new double[]{3.0, 4.0, 5.0},
                new double[]{5.0, 12.0, 13.0},
                new double[]{6.0, 8.0, 10.0},
                new double[]{8.0, 15.0, 17.0},
                new double[]{9.0, 12.0, 15.0},
                new double[]{12.0, 16.0, 20.0},
                new double[]{15.0, 20.0, 25.0},
                new double[]{20.0, 21.0, 29.0}
        );

        // act
        List<double[]> test = MathIsCool.generatePythagorianTriples(1, 22);

        // assert
        assertTrue(Arrays.deepEquals(test.toArray(), label.toArray()));
    }

    @Test
    public void generateFibonacciSeries_Starting1And6Items_CorrectSequence() {
        // arrange
        List<int[]> label = Arrays.asList(
                new int[]{0, 1},
                new int[]{1, 1},
                new int[]{1, 2},
                new int[]{2, 3},
                new int[]{3, 5},
                new int[]{5, 8}
        );

        // act
        List<int[]> test = MathIsCool.generateFibonacciSeries(0, 6);

        // assert
        assertTrue(Arrays.deepEquals(test.toArray(), label.toArray()));
    }

}