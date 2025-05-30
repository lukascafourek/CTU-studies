package cz.cvut.fel.sin.sintest.controller.config;

import lombok.extern.log4j.Log4j2;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;
import cz.cvut.fel.sin.sintest.exception.*;

@ControllerAdvice
@Log4j2
public class RestResponseEntityExceptionHandler extends ResponseEntityExceptionHandler {

    public static final String NOT_FOUND = "NOT_FOUND";
    public static final String FIELD_MISSING = "FIELD_MISSING";
    public static final String FIELD_INVALID = "FIELD_INVALID";
    public static final String FIELD_TOO_LONG = "FIELD_TOO_LONG";
    public static final String ALREADY_EXISTS = "ALREADY_EXISTS";
    public static final String VALUE_NOT_SAME = "VALUE_NOT_SAME";
    public static final String FIGHT_CANNOT_HAPPEN = "FIGHT_CANNOT_HAPPEN";

    @ExceptionHandler(value = {NotFoundException.class})
    protected ResponseEntity<Object> handleNotFoundException(NotFoundException ex) {
        log.error(NOT_FOUND + ": {}", ex.getMessage());
        return badRequest(NOT_FOUND + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {FieldMissingException.class})
    protected ResponseEntity<Object> handleFieldMissingException(FieldMissingException ex) {
        log.error(FIELD_MISSING + ": {}", ex.getMessage());
        return badRequest(FIELD_MISSING + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {FieldInvalidException.class})
    protected ResponseEntity<Object> handleFieldInvalidException(FieldInvalidException ex) {
        log.error(FIELD_INVALID + ": {}", ex.getMessage());
        return badRequest(FIELD_INVALID + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {FieldTooLongException.class})
    protected ResponseEntity<Object> handleFieldTooLongException(FieldTooLongException ex) {
        log.error(FIELD_TOO_LONG + ": {}", ex.getMessage());
        return badRequest(FIELD_TOO_LONG + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {AlreadyExistsException.class})
    protected ResponseEntity<Object> handleAlreadyExistsException(AlreadyExistsException ex) {
        log.error(ALREADY_EXISTS + ": {}", ex.getMessage());
        return badRequest(ALREADY_EXISTS + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {ValueNotSameException.class})
    protected ResponseEntity<Object> handleValueNotSameException(ValueNotSameException ex) {
        log.error(VALUE_NOT_SAME + ": {}", ex.getMessage());
        return badRequest(VALUE_NOT_SAME + ": " + ex.getMessage());
    }

    @ExceptionHandler(value = {FightCannotHappenException.class})
    protected ResponseEntity<Object> handleFightCannotHappenException(FightCannotHappenException ex) {
        log.error(FIGHT_CANNOT_HAPPEN + ": {}", ex.getMessage());
        return badRequest(FIGHT_CANNOT_HAPPEN + ": " + ex.getMessage());
    }

    private ResponseEntity<Object> badRequest(String message) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(message);
    }
}
