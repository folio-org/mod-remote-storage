package org.folio.rs.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.With;

@Data
@With
@AllArgsConstructor
@NoArgsConstructor
public class LogRecordEvent {
  private String logEventType;
  private Object payload;
  private String itemBarcode;
}
