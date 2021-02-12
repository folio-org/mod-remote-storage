package org.folio.rs.repository;

import org.folio.rs.domain.entity.LocationMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface LocationMappingsRepository extends JpaRepository<LocationMapping, UUID> {

  Optional<LocationMapping> getFirstByConfigurationId(UUID configurationId);
}
