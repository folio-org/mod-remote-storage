package org.folio.rs.service;

import static java.util.Objects.isNull;
import static org.folio.rs.domain.entity.ProviderRecord.DEMATIC_SD;
import static org.folio.rs.util.Utils.randomIdAsString;

import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.domain.entity.Configuration;
import org.folio.rs.error.IdMismatchException;
import org.folio.rs.error.RequiredValueMissingException;
import org.folio.rs.mapper.ConfigurationsMapper;
import org.folio.rs.repository.ConfigurationsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ConfigurationsService {

  public static final String CONFIGURATIONS = "configurations";
  private final ConfigurationsRepository configurationsRepository;
  private final ConfigurationsMapper configurationsMapper;

  @CacheEvict(value = CONFIGURATIONS, key = "#id")
  public void deleteConfigurationById(String id) {
    configurationsRepository.deleteById(UUID.fromString(id));
  }

  @Cacheable(value = CONFIGURATIONS, key = "#id")
  public StorageConfiguration getConfigurationById(String id) {
    return configurationsRepository.findById(UUID.fromString(id))
      .map(configurationsMapper::mapEntityToDto)
      .orElse(null);
  }

  public StorageConfigurations getConfigurations(Integer offset, Integer limit) {
    var configurationList = configurationsRepository.findAll(new OffsetRequest(offset, limit));

    return configurationsMapper.mapEntitiesToRemoteConfigCollection(configurationList);
  }

  @CachePut(value = CONFIGURATIONS, key = "#storageConfiguration.id")
  public StorageConfiguration postConfiguration(StorageConfiguration storageConfiguration) {
    if (isNull(storageConfiguration.getAccessionDelay()) && DEMATIC_SD.getId().equals(storageConfiguration.getProviderName())) {
      throw new RequiredValueMissingException("accessionDelay must not be null");
    }
    if (isNull(storageConfiguration.getId())) {
      storageConfiguration.id(randomIdAsString());
    }
    var configuration = configurationsMapper.mapDtoToEntity(storageConfiguration);
    configuration.setCreatedDate(LocalDateTime.now());

    return configurationsMapper.mapEntityToDto(configurationsRepository.save(configuration));
  }

  @CachePut(value = CONFIGURATIONS, key = "#storageConfiguration.id")
  public StorageConfiguration updateConfiguration(String id, StorageConfiguration storageConfiguration) {
    Configuration configuration;
    if (id.equals(storageConfiguration.getId())) {
      var conf = configurationsMapper.mapDtoToEntity(storageConfiguration);
      configuration = configurationsRepository.save(copyForUpdate(configurationsRepository.getOne(conf.getId()), conf));
    } else {
      throw new IdMismatchException();
    }
    return configurationsMapper.mapEntityToDto(configuration);
  }

  private Configuration copyForUpdate(Configuration dest, Configuration source) {
    dest.setProviderName(source.getProviderName());
    dest.setName(source.getName());
    dest.setUrl(source.getUrl());
    dest.setStatusUrl(source.getStatusUrl());
    dest.setApiKey(source.getApiKey());
    dest.setAccessionDelay(source.getAccessionDelay());
    dest.setAccessionTimeUnit(source.getAccessionTimeUnit());
    dest.setUpdatedByUserId(source.getUpdatedByUserId());
    dest.setUpdatedByUsername(source.getUpdatedByUsername());
    dest.setAccessionWorkflowDetails(source.getAccessionWorkflowDetails());
    dest.setReturningWorkflowDetails(source.getReturningWorkflowDetails());
    dest.setUpdatedDate(LocalDateTime.now());
    return dest;
  }
}
