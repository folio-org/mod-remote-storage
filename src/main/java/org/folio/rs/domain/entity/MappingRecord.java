package org.folio.rs.domain.entity;

import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.UUID;

@Data
@Table(name = "LOCATION_MAPPINGS")
@Entity
public class MappingRecord {
  @Id
  @Column(name = "folio_location_id")
  private UUID folioLocationId;

  @NotNull
  @Column(name = "configuration_id")
  private UUID configurationId;
}
