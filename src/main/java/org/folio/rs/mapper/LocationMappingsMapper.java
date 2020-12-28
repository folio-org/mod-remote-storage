package org.folio.rs.mapper;

import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.domain.entity.MappingRecord;
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
  LocationMapping mapEntityToDto(MappingRecord mappingRecord);

  @Mappings({
    @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getFolioLocationId()))"),
    @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(locationMapping.getConfigurationId()))")
  })
  MappingRecord mapDtoToEntity(LocationMapping locationMapping);

  @Mappings({})
  List<LocationMapping> mapEntitiesToDtos(Iterable<MappingRecord> mappingRecords);

  default LocationMappings mapEntitiesToMappingCollection(Iterable<MappingRecord> mappingRecords) {
    List<LocationMapping> mappings = mapEntitiesToDtos(mappingRecords);
    return new LocationMappings().mappings(mappings).totalRecords(mappings.size());
  }
}
