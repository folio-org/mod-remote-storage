package org.folio.rs.mapper;

import java.util.List;
import java.util.stream.Collectors;
import org.folio.rs.domain.dto.AccessionQueue;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;
import org.springframework.data.domain.Page;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface AccessionQueueMapper {

  @Mappings({
      @Mapping(target = "id", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(accessionQueueRecord.getId()))"),
      @Mapping(target = "itemBarcode", source = "itemBarcode"),
      @Mapping(target = "createdDateTime", source = "createdDateTime"),
      @Mapping(target = "accessionedDateTime", source = "accessionedDateTime"),
      @Mapping(target = "remoteStorageId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(accessionQueueRecord.getRemoteStorageId()))"),
      @Mapping(target = "callNumber", source = "callNumber"),
      @Mapping(target = "instanceTitle", source = "instanceTitle"),
      @Mapping(target = "instanceAuthor", source = "instanceAuthor")
  })
  AccessionQueue mapEntityToDto(AccessionQueueRecord accessionQueueRecord);

  default AccessionQueues mapEntitiesToAccessionQueueCollection(Page<AccessionQueueRecord> accessionQueueRecords) {
    List<AccessionQueue> accessionQueues = accessionQueueRecords.getContent().stream()
        .map(this::mapEntityToDto)
        .collect(Collectors.toList());
    return new AccessionQueues()
        .accessions(accessionQueues)
        .totalRecords(Math.toIntExact(accessionQueueRecords.getTotalElements()));
  }
}
