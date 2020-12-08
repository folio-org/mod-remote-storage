package org.folio.rs.repository;

import org.folio.rs.domain.entity.RemoteStorageConfiguration;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface RemoteStorageConfigurationsRepository extends JpaRepository<RemoteStorageConfiguration, UUID> {
}
