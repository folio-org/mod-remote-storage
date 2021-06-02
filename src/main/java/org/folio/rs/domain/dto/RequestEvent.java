package org.folio.rs.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.With;

@Data
@With
@AllArgsConstructor
@NoArgsConstructor
public class RequestEvent {
  private String itemBarCode;
  private String pickupServicePointId;
  private String holdId;
  private String requesterId;
  private String requestStatus;
  private String requestNote;
}
