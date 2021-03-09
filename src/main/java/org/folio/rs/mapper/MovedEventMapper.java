package org.folio.rs.mapper;

import org.folio.rs.domain.dto.MovedEvent;
import org.folio.rs.domain.dto.MovedEventRequest;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface MovedEventMapper {

  @Mappings({
      @Mapping(target = "itemBarCode", source = "payload.itemBarcode"),
      @Mapping(target = "pickupServicePointId", source = "payload.requests.updated.pickupServicePointId"),
      @Mapping(target = "holdId", source = "payload.requests.updated.id"),
      @Mapping(target = "requesterId", source = "payload.requests.updated.requesterId"),
      @Mapping(target = "requestStatus", source = "payload.requests.updated.status"),
      @Mapping(target = "requestNote", source = "payload.requests.updated.patronComments"),
      @Mapping(target = "requestType", source = "payload.requests.updated.requestType"),
  })
  MovedEventRequest mapDtoToEntity(MovedEvent movedEvent);
}
