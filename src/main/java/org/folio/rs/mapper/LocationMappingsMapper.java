package org.folio.rs.mapper;

import java.util.List;

import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.domain.entity.LocationMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface LocationMappingsMapper {
  @Mappings({
      @Mapping(target = "finalLocationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(mappingRecord.getFinalLocationId()))"),
      @Mapping(target = "remoteConfigurationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(mappingRecord.getRemoteConfigurationId()))"),
      @Mapping(target = "originalLocationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(mappingRecord.getOriginalLocationId()))") })
  org.folio.rs.domain.dto.LocationMapping mapEntityToDto(LocationMapping mappingRecord);

  @Mappings({
      @Mapping(target = "finalLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getFinalLocationId()))"),
      @Mapping(target = "remoteConfigurationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getRemoteConfigurationId()))"),
      @Mapping(target = "originalLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getOriginalLocationId()))") })
  LocationMapping mapDtoToEntity(org.folio.rs.domain.dto.LocationMapping locationMapping);

  @Mappings({})
  List<org.folio.rs.domain.dto.LocationMapping> mapEntitiesToDtos(Iterable<LocationMapping> mappingRecords);

  default LocationMappings mapEntitiesToMappingCollection(Iterable<LocationMapping> mappingRecords) {
    List<org.folio.rs.domain.dto.LocationMapping> mappings = mapEntitiesToDtos(mappingRecords);
    return new LocationMappings().mappings(mappings).totalRecords(mappings.size());
  }
}
