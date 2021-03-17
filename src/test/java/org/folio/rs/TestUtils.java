package org.folio.rs;

import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class TestUtils {

  private TestUtils() {
  }

  public static final String ITEM_BARCODE = "645398607547";
  public static final ObjectMapper MAPPER = new ObjectMapper();

  public static ObjectNode buildRequestChangedEventPayload(String originalRequestType, String updatedRequestType) {

    var original = MAPPER.createObjectNode();
    original.put("requestType", originalRequestType);

    var updated = MAPPER.createObjectNode();
    updated.put("pickupServicePointId", "73115c06-dbdb-4aa1-9695-d5d19c973170");
    updated.put("id", randomUuidString());
    updated.put("requesterId", "21932a85-bd00-446b-9565-46e0c1a5490b");
    updated.put("status", "Request-status");
    updated.put("patronComments", "Patron comment");
    updated.put("requestType", updatedRequestType);

    var requests = MAPPER.createObjectNode();
    requests.set("original", original);
    requests.set("updated", updated);

    var payload = MAPPER.createObjectNode();
    payload.put("itemBarcode", ITEM_BARCODE);
    payload.set("requests", requests);

    return payload;

  }

  public static ObjectNode buildRequestCreatedEventPayload(String requestType) {

    var mapper = new ObjectMapper();

    var created = mapper.createObjectNode();
    created.put("pickupServicePointId", "73115c06-dbdb-4aa1-9695-d5d19c973170");
    created.put("id", randomUuidString());
    created.put("requesterId", "21932a85-bd00-446b-9565-46e0c1a5490b");
    created.put("status", "Request-status");
    created.put("patronComments", "Patron comment");
    created.put("requestType", requestType);

    var requests = mapper.createObjectNode();
    requests.set("created", created);

    var payload = mapper.createObjectNode();
    payload.put("itemBarcode", ITEM_BARCODE);
    payload.set("requests", requests);

    return payload;
  }

  public static ObjectNode buildBaseEventPayload() {
    var mapper = new ObjectMapper();

    var payload = mapper.createObjectNode();
    payload.put("itemBarcode", ITEM_BARCODE);

    return payload;
  }

  private static String randomUuidString() {
    return UUID.randomUUID().toString();
  }
}
