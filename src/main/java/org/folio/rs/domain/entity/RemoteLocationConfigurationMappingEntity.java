package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
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
  private UUID remoteStorageConfigurationId;

  @ElementCollection(fetch = FetchType.EAGER)
  @CollectionTable(name = "original_locations", joinColumns = @JoinColumn(name = "final_location_id"))
  @Column(name = "original_location_id")
  private Set<UUID> originalLocationIds = new HashSet<>();
}
