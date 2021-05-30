package org.folio.rs.repository;

import org.folio.rs.domain.entity.OriginalLocation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface OriginalLocationsRepository extends JpaRepository<OriginalLocation, UUID> {
  Optional<OriginalLocation> findByOriginalLocationId(UUID originalLocationId);
}
