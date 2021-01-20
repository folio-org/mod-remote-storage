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
@Table(name = "ACCESSION_QUEUE")
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AccessionQueueRecord {

  @Id
  private UUID id;

  @Column(name = "item_barcode")
  private String itemBarcode;

  @Column(name = "created_date_time")
  private LocalDateTime createdDateTime;

  @Column(name = "accessioned_date_time")
  private LocalDateTime accessionedDateTime;

  @Column(name = "remote_storage_id")
  private UUID remoteStorageId;

  @Column(name = "call_number")
  private String callNumber;

  @Column(name = "instance_title")
  private String instanceTitle;

  @Column(name = "instance_author")
  private String instanceAuthor;

  @Column(name = "accessioned")
  private boolean accessioned;
}
