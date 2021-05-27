package org.folio.rs.repository;

import org.folio.rs.domain.entity.PlainMappingEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface PlainMappingsRepository extends JpaRepository<PlainMappingEntity, UUID> {
  Optional<PlainMappingEntity> findByConfigurationId(UUID configurationId);
}
