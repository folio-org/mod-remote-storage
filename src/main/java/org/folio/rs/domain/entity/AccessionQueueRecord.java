package org.folio.rs.domain.entity;

import java.time.LocalDateTime;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Table(name = "ACCESSION_QUEUE")
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AccessionQueueRecord {

  @Id
  private UUID id;

  private String itemBarcode;

  private LocalDateTime createdDateTime;

  private LocalDateTime accessionedDateTime;

  private UUID remoteStorageId;

  @Column(name = "call_number")
  private String callNumber;

  @Column(name = "instance_title")
  private String instanceTitle;

  @Column(name = "instance_author")
  private String instanceAuthor;
}
