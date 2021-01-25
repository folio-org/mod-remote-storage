package org.folio.rs.repository;

import java.util.Optional;
import java.util.UUID;
import org.folio.rs.domain.entity.SystemUserParameters;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface SystemUserParametersRepository extends JpaRepository<SystemUserParameters, UUID> {

  Optional<SystemUserParameters> getFirstByTenantId(String tenantId);
}
