package org.folio.rs.repository;

import java.util.UUID;

import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ReturnRetrievalQueueRepository
    extends JpaRepository<ReturnRetrievalQueueRecord, UUID>, JpaSpecificationExecutor<ReturnRetrievalQueueRecord> {
}
