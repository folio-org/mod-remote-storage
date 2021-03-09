package org.folio.rs.error;

public class PubSubException extends RuntimeException{

  public PubSubException(String message) {
    super(message);
  }

  public PubSubException(String message, Throwable e) {
    super(message, e);
  }
}
