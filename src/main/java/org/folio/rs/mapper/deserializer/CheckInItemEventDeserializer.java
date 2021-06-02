package org.folio.rs.mapper.deserializer;

import java.io.IOException;


import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import org.folio.rs.domain.dto.ItemCheckInPubSubEvent;

public class CheckInItemEventDeserializer extends EventDeserializerBase<ItemCheckInPubSubEvent> {
  @Override
  public ItemCheckInPubSubEvent deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        var documentContext = getDocumentContext(jsonParser);
      return new ItemCheckInPubSubEvent()
        .withItemBarcode(documentContext.read("$.itemBarcode"));
  }
}
