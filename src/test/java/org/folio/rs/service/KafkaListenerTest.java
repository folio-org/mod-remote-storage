package org.folio.rs.service;

import lombok.extern.slf4j.Slf4j;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.integration.KafkaMessageListener;
import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.HttpStatusCodeException;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@Slf4j
@ExtendWith(MockitoExtension.class)
public class KafkaListenerTest {

  @InjectMocks
  private KafkaMessageListener kafkaMessageListener;
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
  }

  @Test
  void testUnauthorizedError() {
    log.info("======= Test Kafka events processing: Re-authorization in Authorization Error Case =======");
    // when
    var exception = new HttpServerErrorException(HttpStatus.valueOf(401));
    doThrow(exception).when(accessionQueueService).processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(HttpStatusCodeException.class, () -> kafkaMessageListener.handleEvents(events));

    // verify
    verify(accessionQueueService, times(2)).processAccessionQueueRecord(any());
  }

  @Test
  void testNonUnauthorizedError() {
    log.info("======= Test Kafka events processing: Skipping Re-authorization in non-Authorization Error Case =======");

    // when
    doThrow(new HttpClientErrorException(HttpStatus.valueOf(500))).when(accessionQueueService)
      .processAccessionQueueRecord(any());

    // then
    var events = getEventsList();
    assertThrows(HttpStatusCodeException.class, () -> kafkaMessageListener.handleEvents(events));

    // verify
    verify(accessionQueueService, times(1)).processAccessionQueueRecord(any());
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
  }

  private List<DomainEvent> getEventsList() {
    return List.of(new DomainEvent());
  }
}
