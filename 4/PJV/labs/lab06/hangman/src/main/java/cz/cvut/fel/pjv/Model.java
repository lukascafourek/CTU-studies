package cz.cvut.fel.pjv;

import java.util.Arrays;
import java.util.Random;

public class Model {
    public final String LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    public final int MAX_MISS = 6;

    private boolean finished = false;
    private int missCount = 0;
    private String hiddenWord;
    private String guessedWord;

    public String getGuessedWord() {
        return guessedWord;
    }

    public int getMissCount() {
        return missCount;
    }

    public boolean isFinished() {
        return finished;
    }

    public void setFinished(boolean finished) {
        this.finished = finished;
    }

    public int incrementMissedCount() {
        return ++missCount;
    }

    public boolean updateGuessedWord(char guess) {
        char[] arr = guessedWord.toCharArray();
        boolean ret = false;
        for (int i = 0; i < hiddenWord.length(); i++) {
            if (guess == hiddenWord.charAt(i)) {
                arr[i] = guess;
                ret = true;
            }
        }
        guessedWord = new String(arr);
        return ret;
    }

    public void initGame(WordSource wordSource) {
        hiddenWord = wordSource.getWord();
        guessedWord = ".".repeat(hiddenWord.length());
    }
}
