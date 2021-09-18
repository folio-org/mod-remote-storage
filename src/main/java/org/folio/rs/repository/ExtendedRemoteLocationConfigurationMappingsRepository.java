package org.folio.rs.repository;

import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ExtendedRemoteLocationConfigurationMappingsRepository extends JpaRepository<RemoteLocationConfigurationMappingEntity, UUID>,
  JpaSpecificationExecutor<RemoteLocationConfigurationMappingEntity> {
  List<RemoteLocationConfigurationMappingEntity> findAllByRemoteStorageConfigurationId(UUID remoteStorageConfigurationId);
}
