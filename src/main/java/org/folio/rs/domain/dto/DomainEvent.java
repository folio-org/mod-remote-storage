package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
@Getter
@JsonIgnoreProperties(ignoreUnknown = true)
public class DomainEvent {

  @JsonProperty("old")
  private Item oldEntity;

  @JsonProperty("new")
  private Item newEntity;

  private DomainEventType type;

  private String tenant;

}
