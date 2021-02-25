package org.folio.rs.error;

import static java.util.Objects.isNull;
import static org.folio.rs.error.ErrorCode.CHECK_IN_ERROR;
import static org.folio.rs.error.ErrorCode.CONSTRAINT_VIOLATION;
import static org.folio.rs.error.ErrorCode.DATE_FORMAT_ERROR;
import static org.folio.rs.error.ErrorCode.ITEM_RETURN_ERROR;
import static org.folio.rs.error.ErrorCode.NOT_FOUND_ERROR;
import static org.folio.rs.error.ErrorCode.UNKNOWN_ERROR;
import static org.folio.rs.error.ErrorCode.VALIDATION_ERROR;
import static org.folio.rs.error.ErrorType.INTERNAL;
import static org.folio.rs.error.ErrorType.UNKNOWN;

import org.folio.rs.domain.dto.Error;
import org.folio.rs.domain.dto.Errors;
import org.folio.rs.domain.dto.Parameter;
import org.postgresql.util.PSQLException;
import org.postgresql.util.ServerErrorMessage;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import javax.persistence.EntityNotFoundException;
import javax.validation.ConstraintViolationException;
import java.time.format.DateTimeParseException;

@ControllerAdvice
public class DefaultErrorHandler {

  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<Errors> handleMethodArgumentNotValidException(final MethodArgumentNotValidException exception) {
    Errors errors = new Errors();
    exception.getBindingResult()
      .getAllErrors().forEach(er -> errors.addErrorsItem(new Error()
        .message(er.getDefaultMessage())
        .code(VALIDATION_ERROR.getDescription())
        .type(INTERNAL.getValue())
        .addParametersItem(new Parameter()
          .key(((FieldError) er).getField())
          .value(String.valueOf(((FieldError) er).getRejectedValue())))));
    errors.setTotalRecords(errors.getErrors().size());
    return ResponseEntity.unprocessableEntity().body(errors);
  }

  @ExceptionHandler(ConstraintViolationException.class)
  public ResponseEntity<Errors> handleConstraintViolation(final ConstraintViolationException exception) {
    Errors errors = new Errors();
    exception.getConstraintViolations().forEach(constraintViolation ->
      errors.addErrorsItem(new Error()
        .message(constraintViolation.getMessage())
        .code(VALIDATION_ERROR.getDescription())
        .type(INTERNAL.getValue())));
    errors.setTotalRecords(errors.getErrors().size());
    return ResponseEntity
      .badRequest()
      .body(errors);
  }

  @ExceptionHandler(IdMismatchException.class)
  public ResponseEntity<Errors> handleIdMismatch(final IdMismatchException exception) {
    Errors errors = new Errors();
    errors.addErrorsItem(new Error()
      .message(exception.getMessage())
      .code(VALIDATION_ERROR.getDescription())
      .type(INTERNAL.getValue()));
    errors.setTotalRecords(1);
    return ResponseEntity
      .badRequest()
      .body(errors);
  }

  @ExceptionHandler(DataIntegrityViolationException.class)
  public ResponseEntity<Errors> handleDataIntegrityViolation(final DataIntegrityViolationException exception) {
    if (exception.getMostSpecificCause() instanceof PSQLException) {
      ServerErrorMessage serverErrorMessage = ((PSQLException) exception.getMostSpecificCause()).getServerErrorMessage();
      return ResponseEntity
        .unprocessableEntity()
        .body(new Errors()
          .addErrorsItem(new Error()
            .message(isNull(serverErrorMessage) ? null : serverErrorMessage.getDetail())
            .code(CONSTRAINT_VIOLATION.getDescription())
            .type(INTERNAL.getValue())));
    } else {
      return buildUnknownErrorResponse(exception.getMessage());
    }
  }

  @ExceptionHandler(CheckInException.class)
  public ResponseEntity<Errors> handleCheckInErrors(final CheckInException exception) {
    Errors errors = new Errors();
    errors.addErrorsItem(new Error()
      .message(exception.getMessage())
      .code(CHECK_IN_ERROR.getDescription())
      .type(INTERNAL.getValue()));
    errors.setTotalRecords(1);
    return ResponseEntity
      .badRequest()
      .body(errors);
  }

  @ExceptionHandler(ItemReturnException.class)
  public ResponseEntity<Errors> handleItemReturnErrors(final ItemReturnException exception) {
    Errors errors = new Errors();
    errors.addErrorsItem(new Error()
      .message(exception.getMessage())
      .code(ITEM_RETURN_ERROR.getDescription())
      .type(INTERNAL.getValue()));
    errors.setTotalRecords(1);
    return ResponseEntity
      .badRequest()
      .body(errors);
  }

  @ExceptionHandler({ NullPointerException.class, IllegalArgumentException.class, IllegalStateException.class })
  public ResponseEntity<Errors> handleInternal(final RuntimeException exception) {
    return buildUnknownErrorResponse(exception.getMessage());
  }

  @ExceptionHandler({ EntityNotFoundException.class })
  public ResponseEntity<Errors> handleNotFoundExceptions(final EntityNotFoundException exception) {
    Errors errors = new Errors();
    errors.addErrorsItem(new Error()
      .message(exception.getMessage())
      .code(NOT_FOUND_ERROR.getDescription())
      .type(INTERNAL.getValue()));
    errors.setTotalRecords(1);
    return ResponseEntity.status(HttpStatus.NOT_FOUND)
      .body(errors);
  }

  @ExceptionHandler({ DateTimeParseException.class })
  public ResponseEntity<Errors> handleDateTimeFormatExceptions(final DateTimeParseException exception) {
    Errors errors = new Errors();
    errors.addErrorsItem(new Error()
      .message(exception.getMessage())
      .code(DATE_FORMAT_ERROR.getDescription())
      .type(INTERNAL.getValue()));
    errors.setTotalRecords(1);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
      .body(errors);
  }

  private ResponseEntity<Errors> buildUnknownErrorResponse(String message) {
    return ResponseEntity
      .status(HttpStatus.INTERNAL_SERVER_ERROR)
      .body(new Errors()
        .addErrorsItem(new Error()
          .message(message)
          .code(UNKNOWN_ERROR.getDescription())
          .type(UNKNOWN.getValue()))
        .totalRecords(1));
  }
}
