package org.folio.rs.domain.entity;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.folio.rs.domain.dto.ItemNote;

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

  private String instanceContributors;

  private String publisher;

  private String publishYear;

  private String publishPlace;

  private String volume;

  private String enumeration;

  private String chronology;

  private String displaySummary;

  private String issn;

  private String isbn;

  private String oclc;

  private String physicalDescription;

  private String materialType;

  private String copyNumber;

  private UUID permanentLocationId;

  @ElementCollection
  @CollectionTable(name = "item_notes", joinColumns = @JoinColumn(name = "item_id"))
  private List<ItemNoteEntity> notes;
}
