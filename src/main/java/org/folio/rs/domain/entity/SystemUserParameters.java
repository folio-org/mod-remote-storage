package org.folio.rs.domain.entity;

import javax.persistence.Entity;
import javax.persistence.Id;
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
  private String id;

  private String password;

  private String okapiToken;

  private String okapiUrl;

  private String tenantId;
}
