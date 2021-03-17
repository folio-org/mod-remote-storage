package org.folio.rs.controller;

import static java.util.Optional.ofNullable;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;

import java.util.Objects;

import javax.validation.Valid;

import org.folio.rs.domain.dto.ChangeRequestEvent;
import org.folio.rs.domain.dto.CreateRequestEvent;
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

  private static final ObjectMapper MAPPER = new ObjectMapper();

  @Override
  public ResponseEntity<String> pubSubHandlersLogRecordEventPost(@Valid PubSubEvent pubSubEvent) {
    if (Objects.nonNull(pubSubEvent)) {
      EventRequest eventRequest = null;
      try {

        var logEventType = pubSubEvent.getLogEventType();
        var payload = MAPPER.writeValueAsString(pubSubEvent.getPayload());

        if (isPagedRequestCreated(logEventType, payload)) {
          eventRequest = MAPPER.readValue(payload, CreateRequestEvent.class);
        }
        if (isRequestChangedToPaged(logEventType, payload)) {
          eventRequest = MAPPER.readValue(payload, ChangeRequestEvent.class);
        }
      } catch (JsonProcessingException e) {
        log.error("Error processing event: {}", pubSubEvent, e);
      }

      ofNullable(eventRequest).ifPresent(retrievalQueueService::processEventRequest);

    }
    return ResponseEntity.noContent()
      .build();
  }

  private boolean isRequestChangedToPaged(String logEventType, String payload) {
    var dc = JsonPath.parse(payload);
    return isRequestChanged(logEventType) &&
      !Objects.equals(dc.read("$.requests.original.requestType"), dc.read("$.requests.updated.requestType"))
      && Objects.equals(PAGE.value(), dc.read("$.requests.updated.requestType"));
  }

  private boolean isRequestChanged(String logEventType) {
    return LogEventType.REQUEST_MOVED.value().equals(logEventType)
        || LogEventType.REQUEST_UPDATED.value().equals(logEventType);
  }

  private boolean isPagedRequestCreated(String logEventType, String payload) {
    var dc = JsonPath.parse(payload);
    return isRequestCreated(logEventType)
      && Objects.equals(PAGE.value(), dc.read("$.requests.created.requestType"));
  }

  private boolean isRequestCreated(String logEventType) {
    return LogEventType.REQUEST_CREATED.value().equals(logEventType)
        || LogEventType.REQUEST_CREATED_THROUGH_OVERRIDE.value().equals(logEventType);
  }
}
