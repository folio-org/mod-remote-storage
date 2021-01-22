package org.folio.rs.domain.entity;

import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SystemUserParameters {

  @Id
  private UUID id;

  private String username;

  private String password;

  private String okapiToken;

  private String okapiUrl;

  private String tenantId;
}
