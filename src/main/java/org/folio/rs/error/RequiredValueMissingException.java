package org.folio.rs.error;

public class RequiredValueMissingException extends RuntimeException {
  public RequiredValueMissingException(String message) {
    super(message);
  }
}
