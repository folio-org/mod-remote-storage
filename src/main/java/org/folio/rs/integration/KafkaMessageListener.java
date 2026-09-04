package org.folio.rs.integration;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.LogRecordEvent;
import org.folio.rs.service.AccessionQueueService;
import org.folio.rs.service.KafkaService;
import org.folio.rs.service.LogRecordEventService;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.http.HttpStatus;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.HttpStatusCodeException;

@Slf4j
@Component
@RequiredArgsConstructor
public class KafkaMessageListener {

  private final AccessionQueueService accessionQueueService;
  private final LogRecordEventService logRecordEventService;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;

  @KafkaListener(
    id = KafkaService.INVENTORY_ITEM_EVENT_LISTENER_ID,
    containerFactory = "kafkaDomainEventListenerContainerFactory",
    topicPattern = "${application.kafka.listener.inventory-item-events.topic-pattern}",
    groupId = "${application.kafka.listener.inventory-item-events.group-id}",
    concurrency = "${application.kafka.listener.inventory-item-events.concurrency}")
  public void handleInventoryItemEvents(List<DomainEvent> events) {
    log.info("Processing resource events from kafka [eventsCount: {}]", events.size());
    try {
      accessionQueueService.processAccessionQueueRecord(events);
    } catch (HttpStatusCodeException fe) {
      if (fe.getStatusCode() == HttpStatus.UNAUTHORIZED) {
        log.warn("Re-authorization attempt due to: {}", fe.getMessage());
        accessionQueueService.processAccessionQueueRecord(events);
      } else {
        log.error("Error processing Kafka event", fe);
        throw fe;
      }
    }
  }

  @KafkaListener(
    id = KafkaService.LOG_RECORD_LISTENER_ID,
    containerFactory = "kafkaLogRecordEventListenerContainerFactory",
    topicPattern = "${application.kafka.listener.log-record-events.topic-pattern}",
    groupId = "${application.kafka.listener.log-record-events.group-id}",
    concurrency = "${application.kafka.listener.log-record-events.concurrency}")
  public void handleLogRecordEvents(List<ConsumerRecord<String, LogRecordEvent>> records) {
    log.info("Received LOG_RECORD events from kafka [eventsCount: {}]", records.size());
    records.forEach(this::handleLogRecordEvent);
  }

  private void handleLogRecordEvent(ConsumerRecord<String, LogRecordEvent> consumerRecord) {
    var tenant = getTenant(consumerRecord);
    if (tenant.isEmpty()) {
      log.warn("Skipping LOG_RECORD event: missing {} header [topic: {}, offset: {}]",
        XOkapiHeaders.TENANT, consumerRecord.topic(), consumerRecord.offset());
      return;
    }
    systemUserScopedExecutionService.executeAsyncSystemUserScoped(tenant.get(),
      () -> logRecordEventService.processEvent(consumerRecord.value()));
  }

  private Optional<String> getTenant(ConsumerRecord<String, LogRecordEvent> consumerRecord) {
    var header = consumerRecord.headers().lastHeader(XOkapiHeaders.TENANT);
    if (Objects.isNull(header)) {
      return Optional.empty();
    }
    return Optional.of(new String(header.value(), StandardCharsets.UTF_8));
  }
}

