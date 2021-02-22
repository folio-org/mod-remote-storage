package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MovedEventRequest {

  private String itemBarCode;
  private String itemStatusName;
  private String pickupServicePointId;
  private String holdId;
  private String requesterId;
  private String requestStatus;
  private String requestNote;
}
