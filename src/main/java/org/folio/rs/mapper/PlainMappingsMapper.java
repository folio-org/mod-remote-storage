package org.folio.rs.mapper;

import org.folio.rs.domain.dto.PlainMapping;
import org.folio.rs.domain.dto.PlainMappings;
import org.folio.rs.domain.entity.PlainMappingEntity;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface PlainMappingsMapper {
  @Mappings({
      @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(entity.getFolioLocationId()))"),
      @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(entity.getConfigurationId()))") })
  PlainMapping mapEntityToDto(PlainMappingEntity entity);

  @Mappings({
      @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(mapping.getFolioLocationId()))"),
      @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(mapping.getConfigurationId()))") })
  PlainMappingEntity mapDtoToEntity(PlainMapping mapping);

  @Mappings({})
  List<PlainMapping> mapEntitiesToDtos(Iterable<PlainMappingEntity> entities);

  default PlainMappings mapEntitiesToPlainMappingCollection(Iterable<PlainMappingEntity> entities) {
    List<PlainMapping> mappings = mapEntitiesToDtos(entities);
    return new PlainMappings().mappings(mappings).totalRecords(mappings.size());
  }
}
