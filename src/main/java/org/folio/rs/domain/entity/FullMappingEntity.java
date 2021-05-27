package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.Set;
import java.util.UUID;

@Data
@Entity
@AllArgsConstructor(staticName = "of")
@NoArgsConstructor
@Table(name = "location_mappings")
public class FullMappingEntity {
  @Id
  @Column(name = "folio_location_id")
  private UUID finalLocationId;

  @NotNull
  @Column(name = "configuration_id")
  private UUID remoteConfigurationId;

  @NotNull
  @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER)
  @JoinColumn(name = "folio_location_id")
  private Set<OriginalLocation> originalLocations;
}
