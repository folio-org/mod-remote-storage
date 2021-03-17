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
    return new ChangeRequestEvent().withItemBarCode(documentContext.read("$.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.requests.updated.pickupServicePointId"))
      .withHoldId(documentContext.read("$.requests.updated.id"))
      .withRequesterId(documentContext.read("$.requests.updated.requesterId"))
      .withRequestStatus(documentContext.read("$.requests.updated.status"))
      .withRequestNote(documentContext.read("$.requests.updated.patronComments"));
  }
}
