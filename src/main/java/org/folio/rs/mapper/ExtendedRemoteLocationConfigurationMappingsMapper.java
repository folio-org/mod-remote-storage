package org.folio.rs.mapper;

import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.domain.entity.ExtendedRemoteLocationConfigurationMappingEntity;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public final class ExtendedRemoteLocationConfigurationMappingsMapper {
  private ExtendedRemoteLocationConfigurationMappingsMapper(){}

  public static ExtendedRemoteLocationConfigurationMappingEntity mapDtoToEntity(ExtendedRemoteLocationConfigurationMapping mapping) {
    return ExtendedRemoteLocationConfigurationMappingEntity.of(
      UUID.fromString(mapping.getFinalLocationId()),
      UUID.fromString(mapping.getRemoteConfigurationId()),
      Set.of(OriginalLocation.of(UUID.fromString(mapping.getFinalLocationId()),
        UUID.fromString(mapping.getOriginalLocationId())))
    );
  }

  public static ExtendedRemoteLocationConfigurationMappings mapEntityToDtoCollection(ExtendedRemoteLocationConfigurationMappingEntity entity) {
    var mappings = mapEntityToDtoList(entity);
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  public static ExtendedRemoteLocationConfigurationMappings mapEntitiesToDtoCollection(Iterable<ExtendedRemoteLocationConfigurationMappingEntity> entities) {
    var mappings = StreamSupport.stream(entities.spliterator(), false)
      .map(ExtendedRemoteLocationConfigurationMappingsMapper::mapEntityToDtoList)
      .flatMap(List::stream)
      .collect(Collectors.toList());
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  private static List<ExtendedRemoteLocationConfigurationMapping> mapEntityToDtoList(ExtendedRemoteLocationConfigurationMappingEntity entity) {
    return entity.getOriginalLocations().stream()
      .map(l -> new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(entity.getFinalLocationId().toString())
        .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
        .originalLocationId(l.getOriginalLocationId().toString()))
      .collect(Collectors.toList());
  }
}
