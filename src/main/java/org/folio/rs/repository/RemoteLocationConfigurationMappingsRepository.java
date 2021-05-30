package org.folio.rs.repository;

import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface RemoteLocationConfigurationMappingsRepository extends JpaRepository<RemoteLocationConfigurationMappingEntity, UUID> {
  Optional<RemoteLocationConfigurationMappingEntity> findByRemoteConfigurationId(UUID remoteConfigurationId);
}
