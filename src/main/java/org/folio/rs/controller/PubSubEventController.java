package org.folio.rs.controller;

import static org.folio.rs.util.MapperUtils.mapJsonToMovedEventRequest;

import java.util.Objects;
import javax.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.MovedEventRequest;
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
  public ResponseEntity<String> pubSubHandlersMovedEventPost(@Valid String event) {
    MovedEventRequest movedEventRequest = mapJsonToMovedEventRequest(event);
    if (Objects.nonNull(movedEventRequest)) {
      retrievalQueueService.processMovedEventRequest(movedEventRequest);
    }
    return ResponseEntity.noContent().build();
  }
}

