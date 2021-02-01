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
import lombok.Getter;
import lombok.NoArgsConstructor;

@Data
@Table(name = "accession_queue")
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

  private String callNumber;

  private String instanceTitle;

  private String instanceAuthor;
}
