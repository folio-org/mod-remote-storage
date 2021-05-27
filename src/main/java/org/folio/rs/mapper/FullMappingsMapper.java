package org.folio.rs.mapper;

import com.google.common.collect.ImmutableSet;
import org.folio.rs.domain.dto.FullMapping;
import org.folio.rs.domain.dto.FullMappings;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.domain.entity.FullMappingEntity;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public final class FullMappingsMapper {
  private FullMappingsMapper(){}

  public static FullMappingEntity mapDtoToEntity(FullMapping fullMapping) {
    return FullMappingEntity.of(
      UUID.fromString(fullMapping.getFinalLocationId()),
      UUID.fromString(fullMapping.getRemoteConfigurationId()),
      ImmutableSet.of(OriginalLocation.of(UUID.fromString(fullMapping.getFinalLocationId()),
        UUID.fromString(fullMapping.getOriginalLocationId())))
    );
  }

  public static FullMappings mapEntityToDto(FullMappingEntity fullMappingEntity) {
    var mappings = mapEntityToDtoList(fullMappingEntity);
    return new FullMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  public static FullMappings mapEntitiesToDtos(Iterable<FullMappingEntity> entities) {
    var mappings = StreamSupport.stream(entities.spliterator(), false)
      .map(FullMappingsMapper::mapEntityToDtoList)
      .flatMap(List::stream)
      .collect(Collectors.toList());
    return new FullMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  private static List<FullMapping> mapEntityToDtoList(FullMappingEntity fullMappingEntity) {
    return fullMappingEntity.getOriginalLocations().stream()
      .map(l -> new FullMapping()
        .finalLocationId(fullMappingEntity.getFinalLocationId().toString())
        .remoteConfigurationId(fullMappingEntity.getRemoteConfigurationId().toString())
        .originalLocationId(l.getOriginalLocationId().toString()))
      .collect(Collectors.toList());
  }
}
