package org.folio.rs.util;

public enum ErrorCode {
  INTERNAL("-1");

  private final String code;

  ErrorCode(String code) {
    this.code = code;
  }

  public String getTypeCode() {
    return code;
  }
}
