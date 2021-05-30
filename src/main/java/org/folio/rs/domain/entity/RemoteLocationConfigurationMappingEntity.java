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
public class RemoteLocationConfigurationMappingEntity {
  @Id
  @Column(name = "final_location_id")
  private UUID finalLocationId;

  @NotNull
  @Column(name = "remote_configuration_id")
  private UUID remoteConfigurationId;
}
