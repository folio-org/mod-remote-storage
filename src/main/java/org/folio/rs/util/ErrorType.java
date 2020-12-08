package org.folio.rs.util;

public enum ErrorType {
  VALIDATION_ERROR("Validation error");

  private final String description;

  ErrorType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
