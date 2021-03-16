package org.folio.rs;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.folio.rs.util.LogEventType;

import java.util.UUID;

public class TestUtils {

  private TestUtils() {
  }

  public static String buildRequestChangedEventPayload(LogEventType logEventType, String originalRequestType,
                                                        String updatedRequestType) {

    var mapper = new ObjectMapper();

    var original = mapper.createObjectNode();
    original.put("requestType", originalRequestType);

    var updated = mapper.createObjectNode();
    updated.put("pickupServicePointId", UUID.randomUUID()
      .toString());
    updated.put("id", UUID.randomUUID()
      .toString());
    updated.put("requesterId", UUID.randomUUID()
      .toString());
    updated.put("status", "Request-status");
    updated.put("patronComments", "Patron comment");
    updated.put("requestType", updatedRequestType);

    var requests = mapper.createObjectNode();
    requests.set("original", original);
    requests.set("updated", updated);

    var payload = mapper.createObjectNode();
    payload.put("itemBarcode", "123456789");
    payload.set("requests", requests);

    var eventPayload = mapper.createObjectNode();
    eventPayload.put("logEventType", logEventType.value());
    eventPayload.set("payload", payload);

    return eventPayload.toPrettyString();

  }

  public static String buildRequestCreatedEventPayload(LogEventType logEventType, String requestType) {

    var mapper = new ObjectMapper();

    var created = mapper.createObjectNode();
    created.put("pickupServicePointId", UUID.randomUUID()
      .toString());
    created.put("id", UUID.randomUUID()
      .toString());
    created.put("requesterId", UUID.randomUUID()
      .toString());
    created.put("status", "Request-status");
    created.put("patronComments", "Patron comment");
    created.put("requestType", requestType);

    var requests = mapper.createObjectNode();
    requests.set("created", created);

    var payload = mapper.createObjectNode();
    payload.put("itemBarcode", "123456789");
    payload.set("requests", requests);

    var eventPayload = mapper.createObjectNode();
    eventPayload.put("logEventType", logEventType.value());
    eventPayload.set("payload", payload);

    return eventPayload.toPrettyString();

  }

  public static String buildBaseEventPayload(LogEventType logEventType) {

    var mapper = new ObjectMapper();

    var payload = mapper.createObjectNode();
    payload.put("itemBarcode", "123456789");

    var eventPayload = mapper.createObjectNode();
    eventPayload.put("logEventType", logEventType.value());
    eventPayload.set("payload", payload);

    return eventPayload.toPrettyString();
  }
}
