package org.folio.rs.error;

public class CheckInException extends RuntimeException{
  public CheckInException(String message) {
    super(message);
  }
}
