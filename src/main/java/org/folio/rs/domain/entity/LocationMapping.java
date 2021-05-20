package org.folio.rs.domain.entity;

import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.UUID;

@Data
@Entity
@Table(name = "location_mappings")
public class LocationMapping {
  @Id
  private UUID folioLocationId;

  @NotNull
  private UUID configurationId;
}
