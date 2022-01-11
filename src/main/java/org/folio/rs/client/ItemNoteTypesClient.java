package org.folio.rs.client;

import org.folio.rs.domain.dto.ItemNoteType;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "item-note-types")
public interface ItemNoteTypesClient {
  @GetMapping("/{id}")
  ItemNoteType getItemNoteTypeById(@PathVariable String id);
}
