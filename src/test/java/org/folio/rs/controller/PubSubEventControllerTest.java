package org.folio.rs.controller;

import static org.folio.rs.TestUtils.buildBaseEventPayload;
import static org.folio.rs.TestUtils.buildRequestChangedEventPayload;
import static org.folio.rs.TestUtils.buildRequestCreatedEventPayload;
import static org.folio.rs.domain.dto.Request.RequestType.HOLD;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.EventRequest;
import org.folio.rs.domain.dto.PubSubEvent;
import org.folio.rs.service.RetrievalQueueService;
import org.folio.rs.util.LogEventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import lombok.extern.log4j.Log4j2;

@ExtendWith(MockitoExtension.class)
@Log4j2
public class PubSubEventControllerTest extends TestBase {

  private PubSubEventController controller;
  @Mock
  private RetrievalQueueService retrievalQueueService;
  @Mock
  private EventRequest eventRequest;

  @BeforeEach
  void prepare() {
    controller = new PubSubEventController(retrievalQueueService);
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_MOVED", "REQUEST_UPDATED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldProcessChangedEvent(LogEventType logEventType) {
    log.info("=== Should process moved event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setEventPayload(buildRequestChangedEventPayload(logEventType, HOLD.value(), PAGE.value()));
    controller.pubSubHandlersLogRecordEventPost(pubSubEvent);
    verify(retrievalQueueService).processEventRequest(isA(EventRequest.class));
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_MOVED", "REQUEST_UPDATED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessChangedEvent(LogEventType logEventType) {
    log.info("=== Should not process changed event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setEventPayload(buildRequestChangedEventPayload(logEventType, PAGE.value(), PAGE.value()));
    controller.pubSubHandlersLogRecordEventPost(pubSubEvent);
    verify(retrievalQueueService, never()).processEventRequest(any(EventRequest.class));
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_CREATED",
      "REQUEST_CREATED_THROUGH_OVERRIDE" }, mode = EnumSource.Mode.INCLUDE)
  void shouldProcessCreatedEvent(LogEventType logEventType) {
    log.info("=== Should process changed event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setEventPayload(buildRequestCreatedEventPayload(logEventType, PAGE.value()));
    controller.pubSubHandlersLogRecordEventPost(pubSubEvent);
    verify(retrievalQueueService).processEventRequest(isA(EventRequest.class));
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_CREATED",
      "REQUEST_CREATED_THROUGH_OVERRIDE" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessCreatedEvent(LogEventType logEventType) {
    log.info("=== Should not process created event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setEventPayload(buildRequestCreatedEventPayload(logEventType, HOLD.value()));
    controller.pubSubHandlersLogRecordEventPost(pubSubEvent);
    verify(retrievalQueueService, never()).processEventRequest(any(EventRequest.class));
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "LOAN", "NOTICE", "CHECK_IN", "CHECK_OUT",
      "REQUEST_REORDERED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessOtherEventTypes(LogEventType logEventType) {
    log.info("=== Should not process event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setEventPayload(buildBaseEventPayload(logEventType));
    controller.pubSubHandlersLogRecordEventPost(pubSubEvent);
    verify(retrievalQueueService, never()).processEventRequest(any(EventRequest.class));
  }

  @ParameterizedTest
  @EnumSource(LogEventType.class)
  void shouldNotProcessMovedEventWhenEventIsNull(LogEventType logEventType) {
    log.info("=== Should not process moved event ===");
    controller.pubSubHandlersLogRecordEventPost(null);
    verify(retrievalQueueService, never()).processEventRequest(isA((EventRequest.class)));
  }
}
