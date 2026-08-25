package org.folio.rs.service;

import static org.folio.rs.domain.dto.Request.RequestType.HOLD;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;
import static org.folio.rs.util.LogEventType.REQUEST_CREATED;
import static org.folio.rs.util.LogEventType.REQUEST_CREATED_THROUGH_OVERRIDE;
import static org.folio.rs.util.LogEventType.REQUEST_MOVED;
import static org.folio.rs.util.LogEventType.REQUEST_UPDATED;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Map;

import org.folio.rs.domain.dto.LogRecordEvent;
import org.folio.rs.util.LogEventType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.fasterxml.jackson.databind.ObjectMapper;

@ExtendWith(MockitoExtension.class)
public class LogRecordEventServiceTest {

  private static final String CHECK_IN_EVENT = "CHECK_IN_EVENT";
  private static final String OTHER_EVENT_TYPE = "LOAN";
  private static final String BARCODE_001 = "BARCODE-001";
  private static final String BARCODE_002 = "BARCODE-002";
  private static final String HOLD_ID_001 = "hold-001";
  private static final String HOLD_ID_002 = "hold-002";
  private static final String REQUESTER_ID_001 = "user-001";
  private static final String REQUESTER_ID_002 = "user-002";
  private static final String PICKUP_SP_ID_001 = "sp-001";
  private static final String PICKUP_SP_ID_002 = "sp-002";
  private static final String STATUS_OPEN = "Open - Not yet filled";
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @InjectMocks
  private LogRecordEventService logRecordEventService;

  @Mock
  private ReturnRetrievalQueueService returnRetrievalQueueService;

  @Mock
  private ReturnItemService returnItemService;

  @Test
  void processEventCheckInDelegatesToReturnItemService() {
    // when
    var event = new LogRecordEvent(CHECK_IN_EVENT, null, BARCODE_001);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnItemService, times(1)).returnItem(BARCODE_001);
    verify(returnRetrievalQueueService, never()).processEventRequest(any());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class,
    names = { "REQUEST_CREATED", "REQUEST_CREATED_THROUGH_OVERRIDE" },
    mode = EnumSource.Mode.INCLUDE)
  void processEventPageRequestCreatedDelegatesToRetrievalQueue(LogEventType type) {
    // when
    var payload = buildCreatedPayload(PAGE.value(), BARCODE_001, HOLD_ID_001, REQUESTER_ID_001, PICKUP_SP_ID_001);
    var event = new LogRecordEvent(type.value(), payload, null);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnRetrievalQueueService, times(1)).processEventRequest(
      argThat(re -> BARCODE_001.equals(re.getItemBarCode()) && HOLD_ID_001.equals(re.getHoldId())));
    verify(returnItemService, never()).returnItem(any());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class,
    names = { "REQUEST_CREATED", "REQUEST_CREATED_THROUGH_OVERRIDE" },
    mode = EnumSource.Mode.INCLUDE)
  void processEventNonPageRequestCreatedDoesNotDelegate(LogEventType type) {
    // when
    var payload = buildCreatedPayload(HOLD.value(), BARCODE_001, HOLD_ID_001, REQUESTER_ID_001, PICKUP_SP_ID_001);
    var event = new LogRecordEvent(type.value(), payload, null);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnRetrievalQueueService, never()).processEventRequest(any());
    verify(returnItemService, never()).returnItem(any());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class,
    names = { "REQUEST_MOVED", "REQUEST_UPDATED" },
    mode = EnumSource.Mode.INCLUDE)
  void processEventRequestChangedToPageDelegatesToRetrievalQueue(LogEventType type) {
    // when
    var payload = buildChangedPayload(HOLD.value(), PAGE.value(), BARCODE_002, HOLD_ID_002, REQUESTER_ID_002,
      PICKUP_SP_ID_002);
    var event = new LogRecordEvent(type.value(), payload, null);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnRetrievalQueueService, times(1)).processEventRequest(
      argThat(re -> BARCODE_002.equals(re.getItemBarCode()) && HOLD_ID_002.equals(re.getHoldId())));
    verify(returnItemService, never()).returnItem(any());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class,
    names = { "REQUEST_MOVED", "REQUEST_UPDATED" },
    mode = EnumSource.Mode.INCLUDE)
  void processEventRequestTypeUnchangedDoesNotDelegate(LogEventType type) {
    // when
    var payload = buildChangedPayload(PAGE.value(), PAGE.value(), BARCODE_002, HOLD_ID_002, REQUESTER_ID_002,
      PICKUP_SP_ID_002);
    var event = new LogRecordEvent(type.value(), payload, null);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnRetrievalQueueService, never()).processEventRequest(any());
    verify(returnItemService, never()).returnItem(any());
  }

  @Test
  void processEventNullEventDoesNotThrow() {
    // when
    // then
    logRecordEventService.processEvent(null);

    // verify
    verify(returnRetrievalQueueService, never()).processEventRequest(any());
    verify(returnItemService, never()).returnItem(any());
  }

  @Test
  void processEventOtherEventTypeDoesNotDelegate() {
    // when
    var event = new LogRecordEvent(OTHER_EVENT_TYPE, null, null);

    // then
    logRecordEventService.processEvent(event);

    // verify
    verify(returnRetrievalQueueService, never()).processEventRequest(any());
    verify(returnItemService, never()).returnItem(any());
  }

  private Map<String, Object> buildCreatedPayload(String requestType, String itemBarcode, String holdId,
    String requesterId, String pickupServicePointId) {
    var created = MAPPER.createObjectNode();
    created.put("pickupServicePointId", pickupServicePointId);
    created.put("id", holdId);
    created.put("requesterId", requesterId);
    created.put("status", STATUS_OPEN);
    created.put("patronComments", "");
    created.put("requestType", requestType);

    var requests = MAPPER.createObjectNode();
    requests.set("created", created);

    var payload = MAPPER.createObjectNode();
    payload.put("itemBarcode", itemBarcode);
    payload.set("requests", requests);

    return MAPPER.convertValue(payload, Map.class);
  }

  private Map<String, Object> buildChangedPayload(String originalRequestType, String updatedRequestType,
    String itemBarcode, String holdId, String requesterId, String pickupServicePointId) {
    var original = MAPPER.createObjectNode();
    original.put("requestType", originalRequestType);

    var updated = MAPPER.createObjectNode();
    updated.put("pickupServicePointId", pickupServicePointId);
    updated.put("id", holdId);
    updated.put("requesterId", requesterId);
    updated.put("status", STATUS_OPEN);
    updated.put("patronComments", "");
    updated.put("requestType", updatedRequestType);

    var requests = MAPPER.createObjectNode();
    requests.set("original", original);
    requests.set("updated", updated);

    var payload = MAPPER.createObjectNode();
    payload.put("itemBarcode", itemBarcode);
    payload.set("requests", requests);

    return MAPPER.convertValue(payload, Map.class);
  }
}
