package org.folio.rs.service;

import static org.folio.rs.service.KafkaService.EVENT_LISTENER_ID;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.kafka.listener.MessageListenerContainer;

@SpringBootTest(classes = KafkaService.class)
public class KafkaServiceTest {

  @Autowired
  private KafkaService kafkaService;
  @MockBean
  private KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

  @Test
  void restartEventListeners() {
    var mockListenerContainer = mock(MessageListenerContainer.class);
    when(kafkaListenerEndpointRegistry.getListenerContainer(EVENT_LISTENER_ID)).thenReturn(mockListenerContainer);
    kafkaService.restartEventListeners();
    verify(mockListenerContainer).start();
    verify(mockListenerContainer).stop();
  }
}
