package org.folio.rs.integration;

import java.util.List;

import org.folio.HttpStatus;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.service.AccessionQueueService;
import org.folio.rs.service.KafkaService;
import org.folio.rs.service.SecurityManagerService;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import feign.FeignException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class KafkaMessageListener {

  private final AccessionQueueService accessionQueueService;
  private final SecurityManagerService securityManagerService;
  private final FolioExecutionContext folioExecutionContext;

  @Value("${remote-storage.system-user.username}")
  private String username;

  @Value("${remote-storage.system-user.password}")
  private String password;

  @KafkaListener(id = KafkaService.EVENT_LISTENER_ID, containerFactory = "kafkaListenerContainerFactory", topicPattern = "${application.kafka.listener.events.topic-pattern}", groupId = "${application.kafka.listener.events.group-id}", concurrency = "${application.kafka.listener.events.concurrency}")
  public void handleEvents(List<DomainEvent> events) {
    log.info("Processing resource events from kafka [eventsCount: {}]", events.size());
    try {
      accessionQueueService.processAccessionQueueRecord(events);
    } catch (FeignException fe) {
      if (fe.status() == HttpStatus.HTTP_UNAUTHORIZED.toInt()) {
        log.warn("Re-authorization attempt due to: {}", fe.getMessage());
        securityManagerService.refreshSystemUserApiKey(username, password, folioExecutionContext.getOkapiUrl(), folioExecutionContext.getTenantId());
        accessionQueueService.processAccessionQueueRecord(events);
      } else {
        log.error("Error processing Kafka event", fe);
        throw fe;
      }
    }
  }
}
