package org.folio.rs.mapper.deserializer;

import java.io.IOException;

import org.folio.rs.domain.dto.CreateRequestEvent;
import org.folio.rs.domain.dto.EventRequest;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;

public class CreateRequestEventDeserializer extends RequestEventDeserializerBase {

  @Override
  public EventRequest deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
    var documentContext = getDocumentContext(jsonParser);
    return new CreateRequestEvent().withItemBarCode(documentContext.read("$.payload.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.payload.requests.created.pickupServicePointId"))
      .withHoldId(documentContext.read("$.payload.requests.created.id"))
      .withRequesterId(documentContext.read("$.payload.requests.created.requesterId"))
      .withRequestStatus(documentContext.read("$.payload.requests.created.status"))
      .withRequestNote(documentContext.read("$.payload.requests.created.patronComments"));
  }
}
