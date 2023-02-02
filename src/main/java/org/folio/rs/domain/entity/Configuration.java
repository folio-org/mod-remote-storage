package org.folio.rs.domain.entity;

import java.time.LocalDateTime;
import java.util.UUID;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;

import org.folio.rs.domain.dto.AccessionWorkflowDetails;
import org.folio.rs.domain.dto.ReturningWorkflowDetails;
import org.folio.rs.domain.entity.converters.AccessionWorkflowDetailsConverter;
import org.folio.rs.domain.entity.converters.ReturningWorkflowDetailsConverter;

import lombok.Data;
import org.mapstruct.control.DeepClone;

@Data
@Table(name = "remote_storage_configurations")
@Entity
public class Configuration {

  @Id
  private UUID id;

  @Column(name = "name", unique = true)
  @NotNull
  private String name;

  @Column(name = "api_key")
  private String apiKey;

  @Column(name = "provider_name")
  private String providerName;

  @Column(name = "url")
  private String url;

  @Column(name = "status_url")
  private String statusUrl;

  @Column(name = "accession_delay")
  private Integer accessionDelay;

  @Column(name = "accession_time_unit")
  private String accessionTimeUnit;

  @Column(name = "accession_workflow_details")
  @Convert(converter = AccessionWorkflowDetailsConverter.class)
  private AccessionWorkflowDetails accessionWorkflowDetails;

  @Column(name = "returning_workflow_details")
  @Convert(converter = ReturningWorkflowDetailsConverter.class)
  private ReturningWorkflowDetails returningWorkflowDetails;

  @Column(name = "created_date")
  private LocalDateTime createdDate;

  @Column(name = "updated_date")
  private LocalDateTime updatedDate;

  @Column(name = "created_by_user_id")
  private UUID createdByUserId;

  @Column(name = "updated_by_user_id")
  private UUID updatedByUserId;

  @Column(name = "created_by_username")
  private String createdByUsername;

  @Column(name = "updated_by_username")
  private String updatedByUsername;
}
