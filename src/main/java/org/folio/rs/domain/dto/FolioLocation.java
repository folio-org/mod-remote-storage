package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
@JsonIgnoreProperties(ignoreUnknown = true)
public class FolioLocation {
  private String id;
  private String code;
  private String primaryServicePoint;
}
