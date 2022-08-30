package org.folio.rs.domain.entity;

import lombok.Getter;

@Getter
public enum ProviderRecord {
  DEMATIC_EMS("Dematic EMS (API)", "DEMATIC_EMS"),
  DEMATIC_SD("Dematic StagingDirector (TCP/IP)", "DEMATIC_SD"),
  CAIA_SOFT("CaiaSoft", "CAIA_SOFT");

  private final String name;
  private final String id;

  ProviderRecord(String name, String id) {
    this.name = name;
    this.id = id;
  }
}
