package org.folio.rs.domain.entity;

import java.util.UUID;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import lombok.Data;

@Data
@Entity
@Table(name = "location_mappings")
public class LocationMapping {
  @Id
  private UUID finalLocationId;

  @NotNull
  private UUID remoteConfigurationId;

  @NotNull
  private UUID originalLocationId;
}
