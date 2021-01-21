package org.folio.rs.repository;

import org.folio.rs.domain.entity.SystemUserParameters;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CredentialsRepository extends JpaRepository<SystemUserParameters, String> {

}
