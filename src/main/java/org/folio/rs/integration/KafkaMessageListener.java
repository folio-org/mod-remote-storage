package org.folio.rs.integration;

import java.util.List;

import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.repository.SystemUserParametersRepository;
import org.folio.rs.service.AccessionQueueService;
import org.folio.rs.service.KafkaService;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class KafkaMessageListener {

  private final AccessionQueueService accessionQueueService;
  private final SystemUserParametersRepository systemUserParametersRepository;

  @KafkaListener(
    id = KafkaService.EVENT_LISTENER_ID,
    containerFactory = "kafkaListenerContainerFactory",
    topicPattern = "${application.kafka.listener.events.topic-pattern}",
    groupId = "${application.kafka.listener.events.group-id}",
    concurrency = "${application.kafka.listener.events.concurrency}")
  public void handleEvents(List<DomainEvent> events) {
    log.info("Processing resource events from kafka [eventsCount: {}]", events.size());
    accessionQueueService.processAccessionQueueRecord(events);
  }
}
