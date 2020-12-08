package org.folio.rs.util;

import static java.util.Objects.isNull;
import static org.folio.rs.util.ErrorCode.INTERNAL;
import static org.folio.rs.util.ErrorType.VALIDATION_ERROR;

import org.folio.rs.domain.dto.Error;
import org.folio.rs.domain.dto.Errors;
import org.folio.rs.domain.dto.Parameter;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;

public class ErrorUtil {
  public static final String CONFIGURATION_NOT_FOUND = "Configuration not found";

  private ErrorUtil(){}

  public static Errors buildValidationError(MethodArgumentNotValidException ex) {
    Errors errors = new Errors();

    ex.getBindingResult().getAllErrors().forEach(er ->
      errors.addErrorsItem(new Error()
        .message(er.getDefaultMessage())
        .type(VALIDATION_ERROR.getDescription())
        .code(INTERNAL.getTypeCode())
        .addParametersItem(new Parameter()
          .key(((FieldError) er).getField())
          .value(objectToString(((FieldError) er).getRejectedValue())))));

    return errors;
  }

  private static String objectToString(Object o) {
    return isNull(o) ? null : o.toString();
  }
}
