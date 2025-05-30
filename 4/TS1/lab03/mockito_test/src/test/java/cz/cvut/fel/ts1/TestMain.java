package cz.cvut.fel.ts1;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class TestMain {

    @Test
    public void testMethodCall() {
        // Vytvoření mock objektu
        Main myMock = mock(Main.class);

        // Definování chování mock objektu
        when(myMock.methodCall()).thenReturn("mocked result");

        // Volání metody na mock objektu
        System.out.println(myMock.methodCall()); // Vypíše "mocked result"

        // Verifikace, že metoda byla zavolána přesně jednou
        verify(myMock, times(1)).methodCall();

        // Vytvoření spy objektu
        Main mySpy = spy(new Main());

        // Definování reálné chování pro vybranou metodu spy objektu
        when(mySpy.methodCall()).thenCallRealMethod();

        // Volání metody na spy objektu
        System.out.println(mySpy.methodCall()); // Vypíše skutečný výsledek

        // Verifikace, že metoda byla zavolána přesně jednou
        verify(mySpy, times(1)).methodCall();
    }

    @Test
    void testMockedStatic() {
        try (MockedStatic<Main> mocked = mockStatic(Main.class)) {
            mocked.when(Main::method).thenReturn("bar");
            assertEquals("bar", Main.method());
            mocked.verify(Main::method, times(1));
        }
        assertEquals("foo", Main.method());
    }

    @Test
    void testMockedConstruction() {
        try (MockedConstruction<?> mocked = mockConstruction(Main.class)) {
            Main foo = new Main();
            when(foo.method2()).thenReturn("bar");
            assertEquals("bar", foo.method2());
            verify(foo).method2();
            assertEquals(1, mocked.constructed().size());
        }
    }
}
