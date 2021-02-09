package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.Map;
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

  @SuppressWarnings("unchecked")
  @JsonCreator()
  public MovedEventRequest(Map<String, Object> body) {
    Map<String, Object> payload = (Map<String, Object>) body.get("payload");
    Map<String, Object> requests = (Map<String, Object>) payload.get("requests");
    Map<String, String> updatedRequest = (Map<String, String>) requests.get("updated");
    this.itemBarCode = (String) payload.get("itemBarcode");
    this.itemStatusName = (String) payload.get("itemStatusName");
    this.pickupServicePointId = updatedRequest.get("pickupServicePointId");
    this.holdId = updatedRequest.get("id");
    this.requesterId = updatedRequest.get("requesterId");
    this.requestStatus = updatedRequest.get("status");
    this.requestNote = updatedRequest.get("patronComments");
  }
}
