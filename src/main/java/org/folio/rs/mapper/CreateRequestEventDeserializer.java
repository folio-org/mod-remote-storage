package org.folio.rs.mapper;

import java.io.IOException;

import org.folio.rs.domain.dto.CreateRequestEvent;
import org.folio.rs.domain.dto.EventRequest;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.jayway.jsonpath.JsonPath;

public class CreateRequestEventDeserializer extends StdDeserializer<EventRequest> {

  public CreateRequestEventDeserializer() {
    this(null);
  }

  public CreateRequestEventDeserializer(Class<?> vc) {
    super(vc);
  }

  @Override
  public EventRequest deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
    var productNode = jsonParser.getCodec()
      .readTree(jsonParser);
    var documentContext = JsonPath.parse(productNode.toString());
    return new CreateRequestEvent().withItemBarCode(documentContext.read("$.payload.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.payload.requests.create.pickupServicePointId"))
      .withHoldId(documentContext.read("$.payload.requests.create.id"))
      .withRequesterId(documentContext.read("$.payload.requests.create.requesterId"))
      .withRequestStatus(documentContext.read("$.payload.requests.create.status"))
      .withRequestNote(documentContext.read("$.payload.requests.create.patronComments"))
      .withRequestStatus(documentContext.read("$.payload.requests.create.requestType"));
  }
}
