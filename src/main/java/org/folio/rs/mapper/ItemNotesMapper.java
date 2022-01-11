package org.folio.rs.mapper;

import org.folio.rs.domain.dto.AccessionQueueNote;
import org.folio.rs.domain.entity.ItemNoteEntity;

public interface ItemNotesMapper {
  AccessionQueueNote toDto(ItemNoteEntity entity);
}
