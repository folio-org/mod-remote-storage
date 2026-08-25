package org.folio.rs.service;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.nio.charset.StandardCharsets;
import java.util.List;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.internals.RecordHeader;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.LogRecordEvent;
import org.folio.rs.integration.KafkaMessageListener;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.HttpStatusCodeException;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@ExtendWith(MockitoExtension.class)
public class KafkaListenerTest {

  private static final String LOG_RECORD_TOPIC = "LOG_RECORD";
  private static final String CHECK_IN_EVENT = "CHECK_IN_EVENT";
  private static final String BARCODE_001 = "BARCODE-001";
  private static final String TEST_TENANT = "test-tenant";

  @InjectMocks
  private KafkaMessageListener kafkaMessageListener;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private AccessionQueueService accessionQueueService;
  @Mock
  private LogRecordEventService logRecordEventService;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;

  @Test
  void handleInventoryItemEventsSuccessfulCaseDelegatesToAccessionQueueService() {
    log.info("======= Test Kafka events processing: Successful Case =======");

    // then
    kafkaMessageListener.handleInventoryItemEvents(getEventsList());

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
  }

  @Test
  void handleInventoryItemEventsUnauthorizedErrorRetriesOnce() {
    log.info("======= Test Kafka events processing: Re-authorization in Authorization Error Case =======");
    // when
    var exception = new HttpServerErrorException(HttpStatus.valueOf(401));
    doThrow(exception).when(accessionQueueService).processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(HttpStatusCodeException.class, () -> kafkaMessageListener.handleInventoryItemEvents(events));

    // verify
    verify(accessionQueueService, times(2)).processAccessionQueueRecord(any());
  }

  @Test
  void handleInventoryItemEventsNonUnauthorizedErrorSkipsReAuthorization() {
    log.info("======= Test Kafka events processing: Skipping Re-authorization in non-Authorization Error Case =======");

    // when
    doThrow(new HttpClientErrorException(HttpStatus.valueOf(500))).when(accessionQueueService)
      .processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(HttpStatusCodeException.class, () -> kafkaMessageListener.handleInventoryItemEvents(events));

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
  }

  @Test
  void handleInventoryItemEventsNonFeignErrorSkipsReAuthorization() {
    log.info("======= Test Kafka events processing: Skipping Re-authorization in non-Feign Error Case =======");

    // when
    doThrow(NullPointerException.class).when(accessionQueueService)
      .processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(NullPointerException.class, () -> kafkaMessageListener.handleInventoryItemEvents(events));

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
  }

  @Test
  void handleLogRecordEventsWithTenantHeaderDelegatesToLogRecordEventService() {
    // when
    var logRecordEvent = new LogRecordEvent(CHECK_IN_EVENT, null, BARCODE_001);
    var record = buildConsumerRecord(logRecordEvent, TEST_TENANT);

    doAnswer(invocation -> {
      ((Runnable) invocation.getArgument(1)).run();
      return null;
    }).when(systemUserScopedExecutionService).executeAsyncSystemUserScoped(eq(TEST_TENANT), any(Runnable.class));

    // then
    kafkaMessageListener.handleLogRecordEvents(List.of(record));

    // verify
    verify(logRecordEventService, times(1)).processEvent(logRecordEvent);
    verify(systemUserScopedExecutionService, times(1)).executeAsyncSystemUserScoped(eq(TEST_TENANT), any(Runnable.class));
  }

  @Test
  void handleLogRecordEventsMissingTenantHeaderSkipsRecord() {
    // when
    var logRecordEvent = new LogRecordEvent(CHECK_IN_EVENT, null, BARCODE_001);
    var record = buildConsumerRecord(logRecordEvent, null);

    // then
    kafkaMessageListener.handleLogRecordEvents(List.of(record));

    // verify
    verify(logRecordEventService, never()).processEvent(any());
    verify(systemUserScopedExecutionService, never()).executeAsyncSystemUserScoped(any(), any());
  }

  private ConsumerRecord<String, LogRecordEvent> buildConsumerRecord(LogRecordEvent event, String tenant) {
    var record = new ConsumerRecord<>(LOG_RECORD_TOPIC, 0, 0L, (String) null, event);
    if (tenant != null) {
      record.headers().add(new RecordHeader(TENANT, tenant.getBytes(StandardCharsets.UTF_8)));
    }
    return record;
  }

  private List<DomainEvent> getEventsList() {
    return List.of(new DomainEvent());
  }
}
