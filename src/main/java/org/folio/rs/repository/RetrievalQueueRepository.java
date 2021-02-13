package org.folio.rs.repository;

import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface RetrievalQueueRepository extends JpaRepository<RetrievalQueueRecord, UUID>, JpaSpecificationExecutor<RetrievalQueueRecord> {
}
