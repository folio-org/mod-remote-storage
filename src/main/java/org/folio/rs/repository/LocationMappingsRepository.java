package org.folio.rs.repository;

import java.util.Optional;
import java.util.UUID;

import org.folio.rs.domain.entity.LocationMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface LocationMappingsRepository extends JpaRepository<LocationMapping, UUID> {

  Optional<LocationMapping> getFirstByRemoteConfigurationId(UUID remoteConfigurationId);
}
