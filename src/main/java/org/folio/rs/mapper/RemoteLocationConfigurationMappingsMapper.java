package org.folio.rs.mapper;

import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface RemoteLocationConfigurationMappingsMapper {
  @Mappings({
      @Mapping(target = "folioLocationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(entity.getFinalLocationId()))"),
      @Mapping(target = "configurationId", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(entity.getRemoteConfigurationId()))") })
  RemoteLocationConfigurationMapping mapEntityToDto(RemoteLocationConfigurationMappingEntity entity);

  @Mappings({
      @Mapping(target = "finalLocationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(mapping.getFolioLocationId()))"),
      @Mapping(target = "remoteConfigurationId", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(mapping.getConfigurationId()))") })
  RemoteLocationConfigurationMappingEntity mapDtoToEntity(RemoteLocationConfigurationMapping mapping);

  @Mappings({})
  List<RemoteLocationConfigurationMapping> mapEntitiesToDtos(Iterable<RemoteLocationConfigurationMappingEntity> entities);

  default RemoteLocationConfigurationMappings mapEntitiesToRemoteLocationConfigurationMappingCollection(Iterable<RemoteLocationConfigurationMappingEntity> entities) {
    List<RemoteLocationConfigurationMapping> mappings = mapEntitiesToDtos(entities);
    return new RemoteLocationConfigurationMappings().mappings(mappings).totalRecords(mappings.size());
  }
}
