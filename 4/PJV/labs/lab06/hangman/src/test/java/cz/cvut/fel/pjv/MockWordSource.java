package cz.cvut.fel.pjv;

public class MockWordSource implements WordSource {
    private final String word;

    public MockWordSource(String word) {
        this.word = word;
    }

    @Override
    public String getWord() {
        return word;
    }
}
