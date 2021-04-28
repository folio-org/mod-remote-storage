package org.folio.rs.domain.entity;

import java.time.LocalDateTime;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
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

  private String instanceCreator;

  private String instanceContributors;

  private String publisher;

  private String publishYear;

  private String publishPlace;

  private String volume;

  private String enumeration;

  private String chronology;

  private String issn;

  private String isbn;

  private String oclc;

  private String physicalDescription;

  private String materialType;

  private String copyNumber;

  private UUID permanentLocationId;
}
