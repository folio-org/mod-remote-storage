package org.folio.rs.domain.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Table(name = "retrieval_queue")
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RetrievalQueueRecord {

  @Id
  private UUID id;
  private String holdId;
  private String itemBarcode;
  private String instanceTitle;
  private String instanceAuthor;
  private String callNumber;
  private String patronBarcode;
  private String patronName;
  private LocalDateTime retrievedDateTime;
  private String pickupLocation;
  private String requestStatus;
  private String requestNote;
  private LocalDateTime createdDateTime;
  private UUID remoteStorageId;
  private String requestType;
}
