package org.folio.rs.error;

public enum ErrorCode {
  UNKNOWN_ERROR("Unknown error"),
  VALIDATION_ERROR("Validation error"),
  CONSTRAINT_VIOLATION("Constraint violation error"),
  CHECK_IN_ERROR("Check in by barcode error"),
  NOT_FOUND_ERROR("Not found"),
  DATE_FORMAT_ERROR("Date format error"),
  ITEM_RETURN_ERROR("Item return error"),
  ACCESSION_ERROR("Accession error");

  private final String description;

  ErrorCode(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
