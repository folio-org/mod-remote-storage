package org.folio.rs.controller;

import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.MovedEvent;
import org.folio.rs.service.RetrievalQueueService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PubSubEventControllerTest extends TestBase {

  private PubSubEventController controller;
  @Mock
  private RetrievalQueueService retrievalQueueService;
  @Mock
  private MovedEvent movedEvent;

  @BeforeEach
  void prepare() {
    controller = new PubSubEventController(retrievalQueueService);
  }

  @Test
  void shouldProcessMovedEvent() {
    controller.pubSubHandlersMovedEventPost(movedEvent);

    verify(retrievalQueueService).processMovedEventRequest(movedEvent);
  }

  @Test
  void shouldNotProcessMovedEventWhenEventIsNull() {
    controller.pubSubHandlersMovedEventPost(null);

    verify(retrievalQueueService, never()).processMovedEventRequest(isA((MovedEvent.class)));
  }
}