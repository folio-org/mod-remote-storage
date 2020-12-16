package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.mapper.ConfigurationsMapper;
import org.folio.rs.repository.ConfigurationsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.UUID;

import static java.util.Objects.isNull;

@Service
@RequiredArgsConstructor
@Log4j2
public class ConfigurationsServiceImpl implements ConfigurationsService {

  private final ConfigurationsRepository configurationsRepository;
  private final ConfigurationsMapper configurationsMapper;

  @Override
  public void deleteConfigurationById(String configId) {
    var id = UUID.fromString(configId);

    configurationsRepository.deleteById(id);
  }

  @Override
  public StorageConfiguration getConfigurationById(String configId) {
    var id = UUID.fromString(configId);

    return configurationsRepository.findById(id).map(configurationsMapper::mapEntityToDto).orElse(null);
  }

  @Override
  public StorageConfigurations getConfigurations(Integer offset, Integer limit, String query) {
    var configurationList = configurationsRepository.findAll(new OffsetRequest(offset, limit));

    return configurationsMapper.mapEntitiesToRemoteConfigCollection(configurationList);
  }

  @Override
  public StorageConfiguration postConfiguration(StorageConfiguration storageConfiguration) {
    if (isNull(storageConfiguration.getId())) {
      storageConfiguration.id(UUID.randomUUID().toString());
    }
    var configuration = configurationsMapper.mapDtoToEntity(storageConfiguration);
    configuration.setCreatedDate(Timestamp.valueOf(LocalDateTime.now()));

    return configurationsMapper.mapEntityToDto(configurationsRepository.save(configuration));
  }

  @Override
  public StorageConfiguration createOrUpdateConfiguration(StorageConfiguration storageConfiguration) {
    var configuration = configurationsMapper.mapDtoToEntity(storageConfiguration);
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
