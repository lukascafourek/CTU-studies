package cz.cvut.fel.pjv;

public class Circle extends Shape2d implements Positionable {

    private double radius;

    private Point center;

//    private double x, y;

    public Circle() {
        this.radius = 1;
    }

    public Circle(double radius) {
        this.radius = radius;
    }

    @Override
    public void setPosition(double x, double y) {
//        this.x = x;
//        this.y = y;
        center.setCoords(x, y);
    }

    @Override
    public void setX(double x) {
//        this.x = x;
        center.setX(x);
    }

    @Override
    public void setY(double y) {
//        this.y = y;
        center.setY(y);
    }

    @Override
    public double getArea() {
        return Math.PI * radius * radius;
    }

    @Override
    public double getCircumference() {
        return 2 * Math.PI * radius;
    }

    public double getRadius() {
        return radius;
    }

    public void setRadius(double radius) {
        this.radius = radius;
    }
}
