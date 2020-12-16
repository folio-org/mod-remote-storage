package org.folio.rs.error;

import static java.util.Objects.isNull;
import static org.folio.rs.error.ErrorCode.CONSTRAINT_VIOLATION;
import static org.folio.rs.error.ErrorCode.UNKNOWN_ERROR;
import static org.folio.rs.error.ErrorCode.VALIDATION_ERROR;
import static org.folio.rs.error.ErrorType.INTERNAL;
import static org.folio.rs.error.ErrorType.UNKNOWN;

import org.folio.rs.domain.dto.Error;
import org.folio.rs.domain.dto.Errors;
import org.folio.rs.domain.dto.Parameter;
import org.hibernate.exception.ConstraintViolationException;
import org.postgresql.util.PSQLException;
import org.postgresql.util.ServerErrorMessage;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;

public class ErrorUtil {
  public static final String CONFIGURATION_NOT_FOUND = "Configuration not found";

  private ErrorUtil(){}

  public static Errors buildErrors(Throwable throwable) {
    Errors errors = new Errors();

    try {
      if (throwable instanceof DataIntegrityViolationException) {
        ConstraintViolationException exception = ((ConstraintViolationException) throwable.getCause());
        ServerErrorMessage serverErrorMessage = ((PSQLException) exception.getSQLException()).getServerErrorMessage();
        errors.addErrorsItem(new Error()
          .message(isNull(serverErrorMessage) ? throwable.getLocalizedMessage() : serverErrorMessage.getDetail())
          .code(CONSTRAINT_VIOLATION.getDescription())
          .type(INTERNAL.getValue()));
      } else if (throwable instanceof MethodArgumentNotValidException) {
        ((MethodArgumentNotValidException) throwable)
          .getBindingResult()
          .getAllErrors().forEach(er -> errors.addErrorsItem(new Error()
          .message(er.getDefaultMessage())
          .code(VALIDATION_ERROR.getDescription())
          .type(INTERNAL.getValue())
          .addParametersItem(new Parameter()
            .key(((FieldError) er).getField())
            .value(String.valueOf(((FieldError) er).getRejectedValue())))));
      } else {
        errors.addErrorsItem(buildUnknownError(throwable.getMessage()));
      }
    } catch (Exception e) {
      errors.addErrorsItem(buildUnknownError(throwable.getMessage()));
    }

    return errors;
  }

  private static Error buildUnknownError(String message) {
    return new Error()
      .message(message)
      .code(UNKNOWN_ERROR.getDescription())
      .type(UNKNOWN.getValue());
  }
}
