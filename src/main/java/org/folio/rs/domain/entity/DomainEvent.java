package org.folio.rs.domain.entity;



import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.folio.rs.domain.dto.Item;

@Data
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
@Getter
@JsonIgnoreProperties(ignoreUnknown = true)
public class DomainEvent {

  private Item oldEntity;

  private Item newEntity;

  private DomainEventType type;

  private String tenant;

}
