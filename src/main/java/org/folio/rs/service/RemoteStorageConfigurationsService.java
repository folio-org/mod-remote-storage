package org.folio.rs.service;

import org.folio.rs.domain.dto.RemoteStorageConfig;
import org.folio.rs.domain.dto.RemoteStorageConfigCollection;

public interface RemoteStorageConfigurationsService {

  void deleteConfigurationById(String configId);

  RemoteStorageConfig getConfigurationById(String configId);

  RemoteStorageConfigCollection getConfigurations(Integer offset, Integer limit, String query);

  RemoteStorageConfig postConfiguration(RemoteStorageConfig remoteConfig);

  RemoteStorageConfig createOrUpdateConfiguration(RemoteStorageConfig remoteConfig);
}
