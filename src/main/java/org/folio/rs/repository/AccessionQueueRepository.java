package org.folio.rs.repository;

import java.util.UUID;

import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface AccessionQueueRepository extends JpaRepository<AccessionQueueRecord, UUID> {
}
