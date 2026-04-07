package org.folio.rs.client;

import org.folio.rs.domain.dto.ItemNoteType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange(value = "item-note-types")
public interface ItemNoteTypesClient {
  @GetExchange("/{id}")
  ItemNoteType getItemNoteTypeById(@PathVariable String id);
}
