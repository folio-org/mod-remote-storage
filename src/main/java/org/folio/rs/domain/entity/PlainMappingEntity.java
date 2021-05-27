package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.UUID;

@Data
@AllArgsConstructor(staticName = "of")
@NoArgsConstructor
@Entity
@Table(name = "location_mappings")
public class PlainMappingEntity {
  @Id
  @Column(name = "folio_location_id")
  private UUID folioLocationId;

  @NotNull
  @Column(name = "configuration_id")
  private UUID configurationId;
}
