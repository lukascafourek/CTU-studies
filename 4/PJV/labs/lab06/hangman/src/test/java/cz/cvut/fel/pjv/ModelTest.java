package cz.cvut.fel.pjv;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ModelTest {

    @Test
    void initGame() {
        Model sut = new Model();
        sut.initGame(new MockWordSource("APPLE"));
        String expected = ".....";
        String actual = sut.getGuessedWord();
        assertEquals(expected,actual);
    }

    @Test
    void updateGuessedWord() {
        Model sut = new Model();
        sut.initGame(new MockWordSource("APPLE"));
        sut.updateGuessedWord('P');
        String expected = ".PP..";
        String actual = sut.getGuessedWord();
        assertEquals(expected,actual);
    }

}