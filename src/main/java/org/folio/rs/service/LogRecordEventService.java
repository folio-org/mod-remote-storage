package org.folio.rs.service;

import static java.util.Optional.ofNullable;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;

import java.util.Objects;

import org.folio.rs.domain.dto.ChangeRequestEvent;
import org.folio.rs.domain.dto.CreateRequestEvent;
import org.folio.rs.domain.dto.LogRecordEvent;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.util.LogEventType;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Service
@RequiredArgsConstructor
public class LogRecordEventService {

  private static final ObjectMapper MAPPER = new ObjectMapper();

  private final ReturnRetrievalQueueService returnRetrievalQueueService;
  private final ReturnItemService returnItemService;

  public void processEvent(LogRecordEvent logRecordEvent) {
    if (Objects.isNull(logRecordEvent)) {
      return;
    }
    var logEventType = logRecordEvent.getLogEventType();
    log.debug("processEvent [logEventType: {}, hasPayload: {}, hasItemBarcode: {}]",
      logEventType, Objects.nonNull(logRecordEvent.getPayload()), Objects.nonNull(logRecordEvent.getItemBarcode()));
    try {
      if (isItemCheckedIn(logEventType)) {
        returnItemService.returnItem(logRecordEvent.getItemBarcode());
      } else {
        processRequestEvent(logEventType, logRecordEvent.getPayload());
      }
    } catch (JsonProcessingException e) {
      log.error("Error processing event [logEventType: {}]", logEventType, e);
    }
  }

  private void processRequestEvent(String logEventType, Object payload) throws JsonProcessingException {
    RequestEvent requestEvent = null;
    var payloadJson = MAPPER.writeValueAsString(payload);
    if (Objects.nonNull(payload) && isPagedRequestCreated(logEventType, payloadJson)) {
      requestEvent = MAPPER.readValue(payloadJson, CreateRequestEvent.class);
    }
    if (Objects.nonNull(payload) && isRequestChangedToPaged(logEventType, payloadJson)) {
      requestEvent = MAPPER.readValue(payloadJson, ChangeRequestEvent.class);
    }
    ofNullable(requestEvent).ifPresent(returnRetrievalQueueService::processEventRequest);
  }

  private boolean isRequestChangedToPaged(String logEventType, String payload) {
    var dc = JsonPath.parse(payload);
    return isRequestChanged(logEventType) &&
      !Objects.equals(dc.read("$.requests.original.requestType"), dc.read("$.requests.updated.requestType"))
      && Objects.equals(PAGE.value(), dc.read("$.requests.updated.requestType"));
  }

  private boolean isRequestChanged(String logEventType) {
    return LogEventType.REQUEST_MOVED.value().equals(logEventType)
      || LogEventType.REQUEST_UPDATED.value().equals(logEventType);
  }

  private boolean isPagedRequestCreated(String logEventType, String payload) {
    var dc = JsonPath.parse(payload);
    return isRequestCreated(logEventType)
      && Objects.equals(PAGE.value(), dc.read("$.requests.created.requestType"));
  }

  private boolean isItemCheckedIn(String logEventType) {
    return LogEventType.CHECK_IN.value().equals(logEventType);
  }

  private boolean isRequestCreated(String logEventType) {
    return LogEventType.REQUEST_CREATED.value().equals(logEventType)
      || LogEventType.REQUEST_CREATED_THROUGH_OVERRIDE.value().equals(logEventType);
  }
}
