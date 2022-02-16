package org.folio.rs.service;

import feign.FeignException;
import feign.Request;
import feign.Response;
import lombok.extern.slf4j.Slf4j;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.integration.KafkaMessageListener;
import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@Slf4j
@ExtendWith(MockitoExtension.class)
public class KafkaListenerTest {

  @InjectMocks
  private KafkaMessageListener kafkaMessageListener;
  @Mock
  private SecurityManagerService securityManagerService;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private AccessionQueueService accessionQueueService;

  @Test
  void testExpectedFlow() {
    log.info("======= Test Kafka events processing: Successful Case =======");

    // then
    kafkaMessageListener.handleEvents(getEventsList());

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
    verify(securityManagerService, times(0)).prepareOrUpdateSystemUser(any(), any(), any(), any());
  }

  @Test
  void testUnauthorizedError() {
    log.info("======= Test Kafka events processing: Re-authorization in Authorization Error Case =======");
    // when
    doThrow(prepareFeignException(401)).when(accessionQueueService)
      .processAccessionQueueRecord(any());
    doNothing().when(securityManagerService)
      .prepareOrUpdateSystemUser(any(), any(), any(), any());

    // then
    var events = getEventsList();
    assertThrows(FeignException.class, () -> kafkaMessageListener.handleEvents(events));

    // verify
    verify(accessionQueueService, times(2)).processAccessionQueueRecord(any());
    verify(securityManagerService, times(1)).prepareOrUpdateSystemUser(any(), any(), any(), any());

  }

  @Test
  void testNonUnauthorizedError() {
    log.info("======= Test Kafka events processing: Skipping Re-authorization in non-Authorization Error Case =======");

    // when
    doThrow(prepareFeignException(500)).when(accessionQueueService)
      .processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(FeignException.class, () -> kafkaMessageListener.handleEvents(events));

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
    verify(securityManagerService, times(0)).prepareOrUpdateSystemUser(any(), any(), any(), any());
  }

  @Test
  void testNonFeignError() {
    log.info("======= Test Kafka events processing: Skipping Re-authorization in non-Feign Error Case =======");

    // when
    doThrow(NullPointerException.class).when(accessionQueueService)
            .processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(NullPointerException.class, () -> kafkaMessageListener.handleEvents(events));

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
    verify(securityManagerService, times(0)).prepareOrUpdateSystemUser(any(), any(), any(), any());
  }

  private List<DomainEvent> getEventsList() {
    return List.of(new DomainEvent());
  }

  private FeignException prepareFeignException(int status) {
    return FeignException.errorStatus("method-key", Response.builder()
      .status(status)
      .request(prepareMockRequest())
      .build());
  }

  private Request prepareMockRequest() {
    return Request.create(Request.HttpMethod.GET, "", new HashMap<>(), new byte[0], Charset.defaultCharset(), null);
  }
}
