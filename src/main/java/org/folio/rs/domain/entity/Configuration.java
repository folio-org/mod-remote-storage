package org.folio.rs.domain.entity;

import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Table(name = "REMOTE_STORAGE_CONFIGURATIONS")
@Entity
public class Configuration {

  @Id
  private UUID id;

  @Column(name = "name", unique = true)
  @NotNull
  private String name;

  @Column(name = "provider_name")
  private String providerName;

  @Column(name = "url")
  private String url;

  @Column(name = "accession_delay")
  private Integer accessionDelay;

  @Column(name = "accession_time_unit")
  private String accessionTimeUnit;

  @Column(name = "created_date")
  private Timestamp createdDate;

  @Column(name = "updated_date")
  private Timestamp updatedDate;

  @Column(name = "created_by_user_id")
  private UUID createdByUserId;

  @Column(name = "updated_by_user_id")
  private UUID updatedByUserId;

  @Column(name = "created_by_username")
  private String createdByUsername;

  @Column(name = "updated_by_username")
  private String updatedByUsername;
}
