package org.folio.rs.domain.entity;

public enum ProviderRecord {
  DEMATIC_EMS("Dematic EMS", "DEMATIC_EMS"),
  DEMATIC_SD("Dematic StagingDirector", "DEMATIC_SD");

  private final String name;
  private final String id;

  ProviderRecord(String name, String id) {
    this.name = name;
    this.id = id;
  }

  public String getName() {
    return name;
  }

  public String getId() {
    return id;
  }
}
