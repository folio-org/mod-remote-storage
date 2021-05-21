package org.folio.rs.repository;

import java.util.Optional;
import java.util.UUID;

import org.folio.rs.domain.entity.LocationMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface LocationMappingsRepository extends JpaRepository<LocationMapping, UUID> {

  Optional<LocationMapping> getFirstByRemoteConfigurationId(UUID remoteConfigurationId);

  @Query("SELECT lm FROM LocationMapping lm WHERE (cast(:originalLocationId as org.hibernate.type.UUIDCharType) is null or " +
    "lm.originalLocationId = :originalLocationId) and lm.remoteConfigurationId = :remoteConfigurationId")
  Optional<LocationMapping> findByOriginalLocationIdAndRemoteConfigurationId(
    @Param("originalLocationId") UUID originalLocationId,
    @Param("remoteConfigurationId") UUID remoteConfigurationId);
}
