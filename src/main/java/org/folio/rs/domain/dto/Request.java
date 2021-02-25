
package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@JsonIgnoreProperties(ignoreUnknown = true)
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Request {
  private String id;
  private RequestType requestType;
  private String patronComments;
  private Status status;
  private Integer position;
  private String requesterId;
  private String pickupServicePointId;

  @AllArgsConstructor
  public enum RequestType {
    HOLD("Hold"),
    RECALL("Recall"),
    PAGE("Page");

    private final String value;

    @JsonValue
    public String value() {
        return this.value;
    }
  }

  @AllArgsConstructor
  public enum Status {
    OPEN_NOT_YET_FILLED("Open - Not yet filled"),
    OPEN_AWAITING_PICKUP("Open - Awaiting pickup"),
    OPEN_IN_TRANSIT("Open - In transit"),
    OPEN_AWAITING_DELIVERY("Open - Awaiting delivery"),
    CLOSED_FILLED("Closed - Filled"),
    CLOSED_CANCELLED("Closed - Cancelled"),
    CLOSED_UNFILLED("Closed - Unfilled"),
    CLOSED_PICKUP_EXPIRED("Closed - Pickup expired");

    private final String value;

    @JsonValue
    public String value() {
        return this.value;
    }
  }
}
