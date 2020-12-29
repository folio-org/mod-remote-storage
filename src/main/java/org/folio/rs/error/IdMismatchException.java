package org.folio.rs.error;

public class IdMismatchException extends RuntimeException {
  public IdMismatchException() {
    super("request id and entity id are not equal");
  }
}
