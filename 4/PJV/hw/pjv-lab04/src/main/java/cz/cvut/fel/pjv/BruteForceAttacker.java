package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class BruteForceAttacker extends Thief {

    private static char[] chars;
    private static char[] guess;
    private boolean ret;
    @Override
    public void breakPassword(int sizeOfPassword) {
        chars = getCharacters();
        guess = new char[sizeOfPassword];
        guessPasswordRecursive(sizeOfPassword, 0);
    }

    public void guessPasswordRecursive(int sizeOfPassword, int index) {
        if (index == sizeOfPassword) {
            if (!ret) {
                ret = tryOpen(guess);
            }
            return;
        }
        if (!ret) {
            for (char character : chars) {
                guess[index] = character;
                guessPasswordRecursive(sizeOfPassword, index + 1);
            }
        }
    }
}
