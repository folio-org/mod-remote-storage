package org.folio.rs.util;

public enum IdentifierType {
  ISBN("8261054f-be78-422d-bd51-4ed9f33c3422"),
  ISSN("913300b2-03ed-469a-8179-c1092c991227"),
  OCLC("439bfbae-75bc-4f74-9fc7-b2a2d47ce3ef");

  private final String id;

  IdentifierType(String id) {
    this.id = id;
  }

  public String getId() {
    return this.id;
  }
}
