package cz.cvut.fel.ts1;

public class Article {

    private final String name;

    private final String date;

    private final String doi;

    public Article(String name, String date, String doi) {
        this.name = name;
        this.date = date;
        this.doi = doi;
    }

    public String getName() {
        return name;
    }

    public String getDate() {
        return date;
    }

    public String getDoi() {
        return doi;
    }
}
