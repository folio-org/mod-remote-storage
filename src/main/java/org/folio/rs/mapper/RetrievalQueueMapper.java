package org.folio.rs.mapper;

import java.util.List;
import java.util.stream.Collectors;

import org.folio.rs.domain.dto.RetrievalQueue;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;
import org.springframework.data.domain.Page;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface RetrievalQueueMapper {

  @Mappings({
      @Mapping(target = "id", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(retrievalQueueRecord.getId()))"),
      @Mapping(target = "holdId", source = "holdId"), @Mapping(target = "itemBarcode", source = "itemBarcode"),
      @Mapping(target = "instanceTitle", source = "instanceTitle"), @Mapping(target = "instanceAuthor", source = "instanceAuthor"),
      @Mapping(target = "callNumber", source = "callNumber"), @Mapping(target = "patronBarcode", source = "patronBarcode"),
      @Mapping(target = "patronName", source = "patronName"), @Mapping(target = "createdDateTime", source = "createdDateTime"),
      @Mapping(target = "pickupLocation", source = "pickupLocation"), @Mapping(target = "requestStatus", source = "requestStatus"),
      @Mapping(target = "requestNote", source = "requestNote"),
      @Mapping(target = "retrievedDateTime", source = "retrievedDateTime"),
      @Mapping(target = "remoteStorageId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(retrievalQueueRecord.getRemoteStorageId()))") })
  RetrievalQueue mapEntityToDto(RetrievalQueueRecord retrievalQueueRecord);

  default RetrievalQueues mapEntitiesToRetrievalQueueCollection(Page<RetrievalQueueRecord> retrievalQueueRecords) {
    List<RetrievalQueue> retrievalQueues = retrievalQueueRecords.getContent()
      .stream()
      .map(this::mapEntityToDto)
      .collect(Collectors.toList());
    return new RetrievalQueues().retrievals(retrievalQueues)
      .totalRecords(Math.toIntExact(retrievalQueueRecords.getTotalElements()));
  }
}
