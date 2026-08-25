package org.folio.rs.service;

import static org.folio.rs.service.KafkaService.INVENTORY_ITEM_EVENT_LISTENER_ID;
import static org.folio.rs.service.KafkaService.LOG_RECORD_LISTENER_ID;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.kafka.listener.MessageListenerContainer;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

@SpringBootTest(classes = KafkaService.class)
public class KafkaServiceTest {

  @Autowired
  private KafkaService kafkaService;
  @MockitoBean
  private KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

  @Test
  void restartEventListenersRestartsInventoryItemAndLogRecordListeners() {
    // when
    var inventoryItemListenerContainer = mock(MessageListenerContainer.class);
    var logRecordListenerContainer = mock(MessageListenerContainer.class);
    when(kafkaListenerEndpointRegistry.getListenerContainer(INVENTORY_ITEM_EVENT_LISTENER_ID))
      .thenReturn(inventoryItemListenerContainer);
    when(kafkaListenerEndpointRegistry.getListenerContainer(LOG_RECORD_LISTENER_ID))
      .thenReturn(logRecordListenerContainer);

    // then
    kafkaService.restartEventListeners();

    // verify
    verify(inventoryItemListenerContainer).stop();
    verify(inventoryItemListenerContainer).start();
    verify(logRecordListenerContainer).stop();
    verify(logRecordListenerContainer).start();
  }
}
