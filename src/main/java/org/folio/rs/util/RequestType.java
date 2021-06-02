package org.folio.rs.util;

public enum RequestType {
  PYR("PYR"),
  REF("REF");

  RequestType(String type) {
    this.type = type;
  }

  private String type;

  public String getType() {
    return type;
  }
}
