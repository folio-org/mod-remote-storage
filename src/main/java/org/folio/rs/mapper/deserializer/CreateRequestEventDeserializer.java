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
    return new CreateRequestEvent().withItemBarCode(documentContext.read("$.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.requests.created.pickupServicePointId"))
      .withHoldId(documentContext.read("$.requests.created.id"))
      .withRequesterId(documentContext.read("$.requests.created.requesterId"))
      .withRequestStatus(documentContext.read("$.requests.created.status"))
      .withRequestNote(documentContext.read("$.requests.created.patronComments"));
  }
}
