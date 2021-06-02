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

  public static ObjectNode buildCheckInEventPayload() {

    var mapper = new ObjectMapper();

    var payload = mapper.createObjectNode();

    payload.put("logEventType", "CHECK_IN_EVENT");

    payload.put("servicePointId", "c4c90014-c8c9-4ade-8f24-b5e313319f4b");
    payload.put("loanId", "3fd2d7aa-a6fe-4794-9d34-837a6bd31a8b");
    payload.put("isLoanClosed", true);
    payload.put("systemReturnDate", "2021-05-18T08:00:28.052Z");
    payload.put("returnDate", "2021-05-18T08:00:28.000Z");
    payload.put("dueDate", "2017-01-19T12:42:21.000Z");

    payload.put("userId", "ab579dc3-219b-4f5b-8068-ab1c7a55c402");
    payload.put("userBarcode", "508444097915063");
    payload.put("itemId", "bb5a6689-c008-4c96-8f8f-b666850ee12d");
    payload.put("itemBarcode", "645398607547");
    payload.put("itemStatusName", "In transit");
    payload.put("holdingsRecordId", "67cd0046-e4f1-4e4f-9024-adf0b0039d09");

    payload.put("instanceId", "a89eccf0-57a6-495e-898d-32b9b2210f2f");
    payload.put("destinationServicePoint", "Circ Desk 1");
    payload.put("source", "ADMINISTRATOR, DIKU");

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
