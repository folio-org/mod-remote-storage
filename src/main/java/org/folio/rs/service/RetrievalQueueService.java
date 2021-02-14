package org.folio.rs.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.mapper.RetrievalQueueMapper;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

@Service
@Log4j2
@RequiredArgsConstructor
public class RetrievalQueueService {

  private static final String ID = "id";
  private static final String ITEM_BARCODE = "itemBarcode";
  private static final String RETRIEVED_DATE_TIME = "retrievedDateTime";
  private static final String REMOTE_STORAGE_ID = "remoteStorageId";
  private static final String REQUEST_DATE_TIME = "createdDateTime";
  private final RetrievalQueueRepository retrievalQueueRepository;
  private final RetrievalQueueMapper retrievalQueueMapper;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();


  public RetrievalQueues getRetrievals(FilterData filterData) {
    var queueRecords = retrievalQueueRepository.findAll(getCriteriaSpecification(filterData),
        new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return retrievalQueueMapper.mapEntitiesToRetrievalQueueCollection(queueRecords);
  }

  public void setRetrievedById(String retrievalQueueId) {
    Optional<RetrievalQueueRecord> retrievalQueue = retrievalQueueRepository.findOne(Specification.where(hasId(retrievalQueueId).and(notRetrievedSpecification())));
    if (retrievalQueue.isPresent()) {
      saveRetrievalQueueWithCurrentDate(retrievalQueue.get());
    } else {
      throw new EntityNotFoundException("Retrieval queue record with id " + retrievalQueueId + " not found");
    }
  }

  public void setRetrievedByBarcode(String barcode) {
    Optional<RetrievalQueueRecord> retrievalQueueRecord = retrievalQueueRepository.findOne(Specification.where(hasBarcode(barcode).and(notRetrievedSpecification())));
    if (retrievalQueueRecord.isPresent()) {
      saveRetrievalQueueWithCurrentDate(retrievalQueueRecord.get());
    } else {
      throw new EntityNotFoundException("Retrieval queue record with item barcode " + barcode + " not found");
    }
  }


  private Specification<RetrievalQueueRecord> getCriteriaSpecification(FilterData filterData){
    return (record, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNotNull(record.get(RETRIEVED_DATE_TIME)));
      }
      if (Objects.nonNull(filterData.getStorageId())) {
        predicates.add(builder.equal(record.get(REMOTE_STORAGE_ID), stringToUUIDSafe(filterData.getStorageId())));
      }
      if (Objects.nonNull(filterData.getCreateDate())) {
        predicates.add(builder.equal(record.get(REQUEST_DATE_TIME), LocalDateTime.parse(filterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveRetrievalQueueWithCurrentDate(RetrievalQueueRecord record) {
    record.setRetrievedDateTime(LocalDateTime.now());
    retrievalQueueRepository.save(record);
  }

  private Specification<RetrievalQueueRecord> hasBarcode(String barcode) {
    return (record, criteria, builder) -> builder.equal(record.get(ITEM_BARCODE), barcode);
  }

  private Specification<RetrievalQueueRecord> notRetrievedSpecification() {
    return (record, criteria, builder) -> builder.isNull(record.get(RETRIEVED_DATE_TIME));
  }

  private Specification<RetrievalQueueRecord> hasId(String id) {
    return (record, criteria, builder) -> builder.equal(record.get(ID), stringToUUIDSafe(id));
  }

}
