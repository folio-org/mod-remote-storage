package org.folio.rs.service;

import static java.util.Objects.isNull;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.RemoteStorageConfig;
import org.folio.rs.domain.dto.RemoteStorageConfigCollection;
import org.folio.rs.mapper.RemoteStorageConfigurationsMapper;
import org.folio.rs.repository.RemoteStorageConfigurationsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class RemoteStorageConfigurationsServiceImpl implements RemoteStorageConfigurationsService {

  private final RemoteStorageConfigurationsRepository configurationsRepository;
  private final RemoteStorageConfigurationsMapper configurationsMapper;

  @Override
  public void deleteConfigurationById(String configId) {
    var id = UUID.fromString(configId);

    configurationsRepository.deleteById(id);
  }

  @Override
  public RemoteStorageConfig getConfigurationById(String configId) {
    var id = UUID.fromString(configId);

    return configurationsRepository.findById(id).map(configurationsMapper::mapEntityToDto).orElse(null);
  }

  @Override
  public RemoteStorageConfigCollection getConfigurations(Integer offset, Integer limit, String query) {
    var configurationList = configurationsRepository.findAll(new OffsetRequest(offset, limit));

    return configurationsMapper.mapEntitiesToRemoteConfigCollection(configurationList);
  }

  @Override
  public RemoteStorageConfig postConfiguration(RemoteStorageConfig remoteConfig) {
    if (isNull(remoteConfig.getId())) {
      remoteConfig.id(UUID.randomUUID().toString());
    }
    var configuration = configurationsMapper.mapDtoToEntity(remoteConfig);
    configuration.setCreatedDate(Timestamp.valueOf(LocalDateTime.now()));

    return configurationsMapper.mapEntityToDto(configurationsRepository.save(configuration));
  }

  @Override
  public RemoteStorageConfig createOrUpdateConfiguration(RemoteStorageConfig remoteConfig) {
    var configuration = configurationsMapper.mapDtoToEntity(remoteConfig);
    if (isNull(configuration.getId())) {
      configuration.setId(UUID.randomUUID());
      if (isNull(configuration.getCreatedDate())) {
        configuration.setCreatedDate(Timestamp.valueOf(LocalDateTime.now()));
      }
    } else {
      configuration = configurationsRepository.getOne(configuration.getId()).copyForUpdate(configuration);
    }
    return configurationsMapper.mapEntityToDto(configurationsRepository.save(configuration));
  }
}
