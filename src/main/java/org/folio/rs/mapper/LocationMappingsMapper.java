package org.folio.rs.mapper;

import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.MappingRecord;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

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
}
