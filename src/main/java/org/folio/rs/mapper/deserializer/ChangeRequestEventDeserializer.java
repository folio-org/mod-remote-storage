package org.folio.rs.mapper.deserializer;

import java.io.IOException;

import org.folio.rs.domain.dto.ChangeRequestEvent;
import org.folio.rs.domain.dto.EventRequest;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;

public class ChangeRequestEventDeserializer extends RequestEventDeserializerBase {

  @Override
  public EventRequest deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
    var documentContext = getDocumentContext(jsonParser);
    return new ChangeRequestEvent().withItemBarCode(documentContext.read("$.payload.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.payload.requests.updated.pickupServicePointId"))
      .withHoldId(documentContext.read("$.payload.requests.updated.id"))
      .withRequesterId(documentContext.read("$.payload.requests.updated.requesterId"))
      .withRequestStatus(documentContext.read("$.payload.requests.updated.status"))
      .withRequestNote(documentContext.read("$.payload.requests.updated.patronComments"));
  }
}
