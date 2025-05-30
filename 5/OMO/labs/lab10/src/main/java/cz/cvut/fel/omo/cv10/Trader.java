package cz.cvut.fel.omo.cv10;

public class Trader {
    private final String name;
    private final String city;

    private final boolean isVegetarian;

    public Trader(String n, String c, boolean isVegetarian) {
        this.name = n;
        this.city = c;
        this.isVegetarian = isVegetarian;
    }

    public String getName() {
        return this.name;
    }

    public String getCity() {
        return this.city;
    }

    public boolean isVegetarian() {
        return isVegetarian;
    }

    public String toString() {
        return "Trader " + this.name + " in " + this.city + " is " + this.isVegetarian + " vegetarian";
    }
}