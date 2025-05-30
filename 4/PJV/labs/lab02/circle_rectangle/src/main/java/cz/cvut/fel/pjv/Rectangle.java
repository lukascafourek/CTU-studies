package cz.cvut.fel.pjv;

public class Rectangle extends Shape2d implements Positionable {

    private double width;

    private double height;

    private Point upperLeft;

    public Rectangle() {
        this.width = 1;
        this.height = 1;
    }

    public Rectangle(double width, double height) {
        this.width = width;
        this.height = height;
    }

    @Override
    public void setPosition(double x, double y) {
        upperLeft.setCoords(x, y);
    }

    @Override
    public void setX(double x) {
        upperLeft.setX(x);
    }

    @Override
    public void setY(double y) {
        upperLeft.setY(y);
    }

    @Override
    public double getArea() {
        return width * height;
    }

    @Override
    public double getCircumference() {
        return 2 * (width + height);
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }
}
