package org.folio.rs.mapper;

import java.util.List;

import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.domain.entity.Configuration;
import org.mapstruct.InheritInverseConfiguration;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.NullValueCheckStrategy;

@Mapper(componentModel = "spring", nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface ConfigurationsMapper {

  @Mappings({ @Mapping(target = "id", expression = "java(org.folio.rs.util.MapperUtils.uuidToStringSafe(configuration.getId()))"),
      @Mapping(target = "name", source = "name"), @Mapping(target = "providerName", source = "providerName"),
      @Mapping(target = "apiKey", source = "apiKey"), @Mapping(target = "url", source = "url"),
      @Mapping(target = "statusUrl", source = "statusUrl"), @Mapping(target = "accessionDelay", source = "accessionDelay"),
      @Mapping(target = "accessionTimeUnit", expression = "java(org.folio.rs.domain.dto.TimeUnits.fromValue(configuration.getAccessionTimeUnit()))"),
      @Mapping(target = "accessionWorkflowDetails", source = "accessionWorkflowDetails"),
      @Mapping(target = "returningWorkflowDetails", source = "returningWorkflowDetails"),
      @Mapping(target = "metadata.createdDate", source = "createdDate"),
      @Mapping(target = "metadata.updatedDate", source = "updatedDate"),
      @Mapping(target = "metadata.createdByUserId", expression = "java(configuration.getCreatedByUserId() == null ? null : String.valueOf(configuration.getCreatedByUserId()))"),
      @Mapping(target = "metadata.updatedByUserId", expression = "java(configuration.getUpdatedByUserId() == null ? null : String.valueOf(configuration.getUpdatedByUserId()))"),
      @Mapping(target = "metadata.createdByUsername", source = "createdByUsername"),
      @Mapping(target = "metadata.updatedByUsername", source = "updatedByUsername") })
  StorageConfiguration mapEntityToDto(Configuration configuration);

  @Mappings({
      @Mapping(target = "id", expression = "java(org.folio.rs.util.MapperUtils.stringToUUIDSafe(storageConfiguration.getId()))"),
      @Mapping(target = "accessionTimeUnit", expression = "java(storageConfiguration.getAccessionTimeUnit() == null ? null : storageConfiguration.getAccessionTimeUnit().toString())"),
      @Mapping(target = "createdByUserId", expression = "java(storageConfiguration.getMetadata() == null ? null : org.folio.rs.util.MapperUtils.stringToUUIDSafe(storageConfiguration.getMetadata().getCreatedByUserId()))"),
      @Mapping(target = "updatedByUserId", expression = "java(storageConfiguration.getMetadata() == null ? null : org.folio.rs.util.MapperUtils.stringToUUIDSafe(storageConfiguration.getMetadata().getUpdatedByUserId()))") })
  @InheritInverseConfiguration
  Configuration mapDtoToEntity(StorageConfiguration storageConfiguration);

  @Mappings({})
  List<StorageConfiguration> mapEntitiesToDtos(Iterable<Configuration> remoteStorageConfigurationList);

  default StorageConfigurations mapEntitiesToRemoteConfigCollection(Iterable<Configuration> remoteStorageConfigurationList) {
    List<StorageConfiguration> remoteConfigList = mapEntitiesToDtos(remoteStorageConfigurationList);
    return new StorageConfigurations().configurations(remoteConfigList)
      .totalRecords(remoteConfigList.size());
  }
}
