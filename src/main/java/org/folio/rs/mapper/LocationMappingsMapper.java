package org.folio.rs.mapper;

import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.domain.entity.LocationMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface LocationMappingsMapper {
  @Mappings({
    @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(mappingRecord.getFolioLocationId()))"),
    @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(mappingRecord.getConfigurationId()))")
  })
  org.folio.rs.domain.dto.LocationMapping mapEntityToDto(LocationMapping mappingRecord);

  @Mappings({
    @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getFolioLocationId()))"),
    @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getConfigurationId()))")
  })
  LocationMapping mapDtoToEntity(org.folio.rs.domain.dto.LocationMapping locationMapping);

  @Mappings({})
  List<org.folio.rs.domain.dto.LocationMapping> mapEntitiesToDtos(Iterable<LocationMapping> mappingRecords);

  default LocationMappings mapEntitiesToMappingCollection(Iterable<LocationMapping> mappingRecords) {
    List<org.folio.rs.domain.dto.LocationMapping> mappings = mapEntitiesToDtos(mappingRecords);
    return new LocationMappings().mappings(mappings).totalRecords(mappings.size());
  }
}
