package cz.fel.sin.library.exception;

public class FieldTooLongException extends RuntimeException {
    public FieldTooLongException(String message) {
        super(message);
    }
}
