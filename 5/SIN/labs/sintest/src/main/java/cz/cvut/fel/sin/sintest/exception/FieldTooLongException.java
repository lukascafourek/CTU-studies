package cz.cvut.fel.sin.sintest.exception;

public class FieldTooLongException extends RuntimeException {
    public FieldTooLongException(String message) {
        super(message);
    }
}
