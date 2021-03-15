package org.folio.rs.mapper;

import java.io.IOException;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;
import org.folio.rs.domain.dto.ChangeRequestEvent;
import org.folio.rs.domain.dto.EventRequest;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.jayway.jsonpath.JsonPath;

public class ChangeRequestEventDeserializer extends StdDeserializer<EventRequest> {

  public ChangeRequestEventDeserializer() {
    this(null);
  }

  public ChangeRequestEventDeserializer(Class<?> vc) {
    super(vc);
  }

  @Override
  public EventRequest deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
    var productNode = jsonParser.getCodec()
      .readTree(jsonParser);
    var conf = Configuration.builder()
      .options(Option.SUPPRESS_EXCEPTIONS).build();
    var documentContext = JsonPath.using(conf).parse(productNode.toString());

    return new ChangeRequestEvent().withItemBarCode(documentContext.read("$.payload.itemBarcode"))
      .withPickupServicePointId(documentContext.read("$.payload.requests.updated.pickupServicePointId"))
      .withHoldId(documentContext.read("$.payload.requests.updated.id"))
      .withRequesterId(documentContext.read("$.payload.requests.updated.requesterId"))
      .withRequestStatus(documentContext.read("$.payload.requests.updated.status"))
      .withRequestNote(documentContext.read("$.payload.requests.updated.patronComments"))
      .withRequestType(documentContext.read("$.payload.requests.updated.requestType"));
  }
}
