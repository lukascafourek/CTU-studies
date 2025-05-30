package cz.cvut.k36.omo.hw.hw01;

public class Main {

    public static void main(String[] args) {
        Homework1 homework1 = new Homework1();
        Homework1 homework2 = new Homework1();
        boolean a = homework1.f();
        System.out.println(a);
        boolean b = Homework1.g();
        System.out.println(b);
        int c = 0, d = 0, e = 0, f = 0;
        for (int i = 0; i < 5; i++) {
            c = homework1.h();
            d = homework1.i();
        }
        System.out.println(c);
        System.out.println(d);
        for (int i = 0; i < 10; i++) {
            e = homework2.h();
            f = homework2.i();
        }
        System.out.println(e);
        System.out.println(f);
    }
}
