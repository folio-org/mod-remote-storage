package org.folio.rs.mapper;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.domain.dto.RemoteStorageConfig;
import org.folio.rs.domain.dto.RemoteStorageConfigCollection;
import org.folio.rs.domain.entity.RemoteStorageConfiguration;
import org.mapstruct.InheritInverseConfiguration;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

import java.util.List;
import java.util.UUID;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface RemoteStorageConfigurationsMapper {

  @Mappings({
    @Mapping(target = "id", expression = "java(uuidToStringSafe(remoteStorageConfiguration.getId()))"),
    @Mapping(target = "name", source = "name"),
    @Mapping(target = "providerName", source = "providerName"),
    @Mapping(target = "url", source = "url"),
    @Mapping(target = "accessionDelay", source = "accessionDelay"),
    @Mapping(target = "accessionTimeUnit", expression = "java(org.folio.rs.domain.dto.TimeUnits.fromValue(remoteStorageConfiguration.getAccessionTimeUnit()))"),
    @Mapping(target = "metadata.createdDate", source = "createdDate"),
    @Mapping(target = "metadata.updatedDate", source = "updatedDate"),
    @Mapping(target = "metadata.createdByUserId", expression = "java(remoteStorageConfiguration.getCreatedByUserId() == null ? null : String.valueOf(remoteStorageConfiguration.getCreatedByUserId()))"),
    @Mapping(target = "metadata.updatedByUserId", expression = "java(remoteStorageConfiguration.getUpdatedByUserId() == null ? null : String.valueOf(remoteStorageConfiguration.getUpdatedByUserId()))"),
    @Mapping(target = "metadata.createdByUsername", source = "createdByUsername"),
    @Mapping(target = "metadata.updatedByUsername", source = "updatedByUsername")
  })
  RemoteStorageConfig mapEntityToDto(RemoteStorageConfiguration remoteStorageConfiguration);

  @Mappings({
    @Mapping(target = "id", expression = "java(stringToUUIDSafe(remoteConfig.getId()))"),
    @Mapping(target = "accessionTimeUnit", expression = "java(remoteConfig.getAccessionTimeUnit() == null ? null : remoteConfig.getAccessionTimeUnit().toString())"),
    @Mapping(target = "createdByUserId", expression = "java(remoteConfig.getMetadata() == null ? null : stringToUUIDSafe(remoteConfig.getMetadata().getCreatedByUserId()))"),
    @Mapping(target = "updatedByUserId", expression = "java(remoteConfig.getMetadata() == null ? null : stringToUUIDSafe(remoteConfig.getMetadata().getUpdatedByUserId()))")
  })
  @InheritInverseConfiguration
  RemoteStorageConfiguration mapDtoToEntity(RemoteStorageConfig remoteConfig);

  @Mappings({})
  List<RemoteStorageConfig> mapEntitiesToDtos(Iterable<RemoteStorageConfiguration> remoteStorageConfigurationList);

  @InheritInverseConfiguration
  List<RemoteStorageConfiguration> mapDtosToEntities(List<RemoteStorageConfig> configs);

  default RemoteStorageConfigCollection mapEntitiesToRemoteConfigCollection(Iterable<RemoteStorageConfiguration> remoteStorageConfigurationList) {
    List<RemoteStorageConfig> remoteConfigList = mapEntitiesToDtos(remoteStorageConfigurationList);
    return new RemoteStorageConfigCollection().configurations(remoteConfigList).totalRecords(remoteConfigList.size());
  }

  default UUID stringToUUIDSafe(String uuid) {
    return (StringUtils.isBlank(uuid)) ? null : java.util.UUID.fromString(uuid);
  }

  default String uuidToStringSafe(UUID uuid) {
    return uuid != null ? uuid.toString() : null;
  }
}
