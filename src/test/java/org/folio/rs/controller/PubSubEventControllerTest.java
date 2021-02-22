package org.folio.rs.controller;

import static org.folio.rs.util.JsonUtils.readJson;
import static org.junit.Assert.assertEquals;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.MovedEventRequest;
import org.folio.rs.service.RetrievalQueueService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.BDDMockito;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PubSubEventControllerTest extends TestBase {

  private static final String REQUEST_MOVED_PAYLOAD_JSON = "payloads/request_moved.json";

  private static PubSubEventController controller;
  @Mock
  private RetrievalQueueService retrievalQueueService;
  @Captor
  private ArgumentCaptor<MovedEventRequest> captor;

  @BeforeEach
  void prepare() {
    controller = new PubSubEventController(retrievalQueueService);
  }

  @Test
  void shouldMapAllRequiredFieldsFromMovedEventJson() {
    controller.pubSubHandlersMovedEventPost(readJson(REQUEST_MOVED_PAYLOAD_JSON));

    BDDMockito.verify(retrievalQueueService).processMovedEventRequest(captor.capture());

    MovedEventRequest event = captor.getValue();
    assertEquals("653285216743", event.getItemBarCode());
    assertEquals("Paged", event.getItemStatusName());
    assertEquals("3a40852d-49fd-4df2-a1f9-6e2641a6e91f", event.getPickupServicePointId());
    assertEquals("7babb1ab-f46f-4c74-8950-bda779440f6f", event.getHoldId());
    assertEquals("e546d50a-926a-421f-8400-a041a2e9db79", event.getRequesterId());
    assertEquals("Open - Not yet filled", event.getRequestStatus());
    assertEquals("request note", event.getRequestNote());
  }
}