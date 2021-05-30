package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;
import java.util.UUID;

@Data
@Entity
@AllArgsConstructor(staticName = "of")
@NoArgsConstructor
@IdClass(OriginalLocationId.class)
@Table(name = "original_locations")
public class OriginalLocation {
  @Id
  @Column(name = "final_location_id")
  private UUID finalLocationId;

  @Id
  @Column(name = "original_location_id")
  private UUID originalLocationId;
}
