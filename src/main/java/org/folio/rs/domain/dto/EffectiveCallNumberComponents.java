package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.With;

@JsonIgnoreProperties(ignoreUnknown = true)
@Getter
@With
@AllArgsConstructor
@NoArgsConstructor
public class EffectiveCallNumberComponents {
  private String callNumber;
}
