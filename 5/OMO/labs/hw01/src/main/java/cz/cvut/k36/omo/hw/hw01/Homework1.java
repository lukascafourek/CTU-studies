package cz.cvut.k36.omo.hw.hw01;

public class Homework1 {

    public Homework1() {}

    private int thisInstanceCount = 0;

    private static int allInstancesCount = 0;

    public boolean f() {
        return true;
    }

    public static boolean g() {
        return false;
    }

    public int h() {
        return ++thisInstanceCount;
    }

    public int i() {
        return ++allInstancesCount;
    }
}
