package org.folio.rs.controller;

import static java.util.Optional.ofNullable;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;

import java.util.Objects;

import javax.validation.Valid;

import com.jayway.jsonpath.DocumentContext;
import org.folio.rs.domain.dto.ChangeRequestEvent;
import org.folio.rs.domain.dto.EventRequest;
import org.folio.rs.domain.dto.PubSubEvent;
import org.folio.rs.rest.resource.PubSubHandlersApi;
import org.folio.rs.service.RetrievalQueueService;
import org.folio.rs.util.LogEventType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Controller
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class PubSubEventController implements PubSubHandlersApi {

  private final RetrievalQueueService retrievalQueueService;

  private final static ObjectMapper MAPPER = new ObjectMapper();

  @Override
  public ResponseEntity<String> pubSubHandlersMovedEventPost(@Valid PubSubEvent pubSubEvent) {
    if (Objects.nonNull(pubSubEvent)) {
      EventRequest eventRequest = null;
      var payload = pubSubEvent.getEventPayload();
      var documentContext = JsonPath.parse(payload);

      try {
        if (isPagedRequestCreated(documentContext)) {
          eventRequest = MAPPER.readValue(payload, ChangeRequestEvent.class);
        }
        if (isRequestChangedToPaged(documentContext)) {
          eventRequest = MAPPER.readValue(payload, ChangeRequestEvent.class);
        }
      } catch (JsonProcessingException e) {
        log.error("Error processing event: {}", payload, e);
      }

      ofNullable(eventRequest).ifPresent(retrievalQueueService::processMovedEventRequest);

    }
    return ResponseEntity.noContent()
      .build();
  }

  private boolean isRequestChangedToPaged(DocumentContext documentContext) {

    var logEventType = documentContext.read("$.logEventType", String.class);
    return isRequestChanged(logEventType)
        && !Objects.equals(documentContext.read("$.payload.requests.original.requestType"), documentContext.read("$.payload.requests.updated.requestType"))
        && Objects.equals(PAGE.value(), documentContext.read("$.payload.requests.updated.requestType"));
  }

  private boolean isRequestChanged(String logEventType) {
    return LogEventType.REQUEST_MOVED.value().equals(logEventType)
        || LogEventType.REQUEST_UPDATED.value().equals(logEventType);
  }

  private boolean isPagedRequestCreated(DocumentContext documentContext) {
    var logEventType = documentContext.read("$.logEventType", String.class);
    return isRequestCreated(logEventType) && Objects.equals(PAGE.value(), documentContext.read("$.payload.requests.created.requestType"));
  }

  private boolean isRequestCreated(String logEventType) {
    return LogEventType.REQUEST_CREATED.value().equals(logEventType)
        || LogEventType.REQUEST_CREATED_THROUGH_OVERRIDE.value().equals(logEventType);
  }
}
