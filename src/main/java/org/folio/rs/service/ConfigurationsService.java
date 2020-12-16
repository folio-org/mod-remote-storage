package org.folio.rs.service;

import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;

public interface ConfigurationsService {

  @CacheEvict("configurations")
  void deleteConfigurationById(String configId);

  @Cacheable("configurations")
  StorageConfiguration getConfigurationById(String configId);

  StorageConfigurations getConfigurations(Integer offset, Integer limit, String query);

  @CachePut("configurations")
  StorageConfiguration postConfiguration(StorageConfiguration storageConfiguration);

  @CachePut("configurations")
  StorageConfiguration createOrUpdateConfiguration(StorageConfiguration storageConfiguration);
}
