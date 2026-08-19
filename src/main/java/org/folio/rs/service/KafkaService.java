package org.folio.rs.service;

import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Service
@RequiredArgsConstructor
public class KafkaService {

  public static final String INVENTORY_ITEM_EVENT_LISTENER_ID = "mod-remote-storage-inventory-item-listener";
  public static final String LOG_RECORD_LISTENER_ID = "mod-remote-storage-log-record-listener";

  private final KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

  /**
   * Restarts kafka event listeners in mod-remote-storage application.
   */
  public void restartEventListeners() {
    log.info("Restarting kafka consumer to start listening created topics [id: {}]", INVENTORY_ITEM_EVENT_LISTENER_ID);
    var listenerContainer = kafkaListenerEndpointRegistry.getListenerContainer(INVENTORY_ITEM_EVENT_LISTENER_ID);
    listenerContainer.stop();
    listenerContainer.start();
  }
}
