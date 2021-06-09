package org.folio.rs.mapper;

import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.springframework.data.domain.Page;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static java.util.Objects.isNull;

public final class RemoteLocationConfigurationMappingsMapper {
  private RemoteLocationConfigurationMappingsMapper(){}

  public static RemoteLocationConfigurationMappingEntity mapMappingDtoToEntity(RemoteLocationConfigurationMapping mapping) {
    return RemoteLocationConfigurationMappingEntity.of(
      UUID.fromString(mapping.getFolioLocationId()),
      UUID.fromString(mapping.getConfigurationId()),
      Collections.emptySet()
    );
  }

  public static RemoteLocationConfigurationMappingEntity mapExtendedMappingDtoToEntity(ExtendedRemoteLocationConfigurationMapping mapping) {
    return   RemoteLocationConfigurationMappingEntity.of(
      UUID.fromString(mapping.getFinalLocationId()),
      UUID.fromString(mapping.getRemoteConfigurationId()),
      Set.of(OriginalLocation.of(null, UUID.fromString(mapping.getFinalLocationId()),
        UUID.fromString(mapping.getOriginalLocationId())))
    );
  }

  public static ExtendedRemoteLocationConfigurationMappings mapEntityToExtendedMappingDtoCollection(RemoteLocationConfigurationMappingEntity entity) {
    var mappings = mapEntityToExtendedMappingDtoList(entity);
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  public static ExtendedRemoteLocationConfigurationMappings mapEntitiesToExtendedMappingsDtoCollection(Page<RemoteLocationConfigurationMappingEntity> entities,
    String originalLocationId) {
    var mappings = StreamSupport.stream(entities.spliterator(), false)
      .map(RemoteLocationConfigurationMappingsMapper::mapEntityToExtendedMappingDtoList)
      .flatMap(List::stream)
      .filter(mapping -> isNull(originalLocationId) || originalLocationId.equals(mapping.getOriginalLocationId()))
      .collect(Collectors.toList());
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords((int)entities.getTotalElements());
  }

  public static RemoteLocationConfigurationMappings mapEntitiesToMappingsDtoCollection(Iterable<RemoteLocationConfigurationMappingEntity> entities) {
    var mappings = StreamSupport.stream(entities.spliterator(), false)
      .map(RemoteLocationConfigurationMappingsMapper::mapEntityToMappingDto)
      .collect(Collectors.toList());
    return new RemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  public static RemoteLocationConfigurationMapping mapEntityToMappingDto(RemoteLocationConfigurationMappingEntity entity) {
    return new RemoteLocationConfigurationMapping()
      .folioLocationId(entity.getFinalLocationId().toString())
      .configurationId(entity.getRemoteConfigurationId().toString());
  }

  private static List<ExtendedRemoteLocationConfigurationMapping> mapEntityToExtendedMappingDtoList(RemoteLocationConfigurationMappingEntity entity) {
    if (entity.getOriginalLocations().isEmpty()) {
      return Collections.singletonList(new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(entity.getFinalLocationId().toString())
        .remoteConfigurationId(entity.getRemoteConfigurationId().toString()));
    }
    return entity.getOriginalLocations().stream()
      .map(l -> new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(entity.getFinalLocationId().toString())
        .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
        .originalLocationId(l.getOriginalLocationId().toString()))
      .collect(Collectors.toList());
  }
}
