package org.folio.rs.controller;

import java.util.Objects;
import javax.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.MovedEvent;
import org.folio.rs.rest.resource.PubSubHandlersApi;
import org.folio.rs.service.RetrievalQueueService;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

@Log4j2
@Controller
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class PubSubEventController implements PubSubHandlersApi {

  private final RetrievalQueueService retrievalQueueService;

  @Override
  public ResponseEntity<String> pubSubHandlersMovedEventPost(@Valid MovedEvent movedEvent) {
    if (Objects.nonNull(movedEvent)) {
      retrievalQueueService.processMovedEventRequest(movedEvent);
    }
    return ResponseEntity.noContent().build();
  }
}

