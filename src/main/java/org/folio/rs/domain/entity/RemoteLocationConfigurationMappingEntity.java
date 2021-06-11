package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Data
@Entity
@AllArgsConstructor(staticName = "of")
@NoArgsConstructor
@Table(name = "location_mappings")
public class RemoteLocationConfigurationMappingEntity {
  @Id
  @Column(name = "final_location_id")
  private UUID finalLocationId;

  @NotNull
  @Column(name = "remote_configuration_id")
  private UUID remoteConfigurationId;

  @ElementCollection(fetch = FetchType.EAGER)
  @CollectionTable(name = "original_locations", joinColumns = @JoinColumn(name = "final_location_id"))
  @Column(name = "original_location_id")
  private Set<UUID> originalLocationIds = new HashSet<>();
}
