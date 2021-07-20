package org.folio.rs.service;

import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Service
@RequiredArgsConstructor
public class KafkaService {

  public static final String EVENT_LISTENER_ID = "mod-remote-storage-events-listener";

  private final KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

  /**
   * Restarts kafka event listeners in mod-remote-storage application.
   */
  public void restartEventListeners() {
    log.info("Restarting kafka consumer to start listening created topics [id: {}]", EVENT_LISTENER_ID);
    var listenerContainer = kafkaListenerEndpointRegistry.getListenerContainer(EVENT_LISTENER_ID);
    listenerContainer.stop();
    listenerContainer.start();
  }
}
