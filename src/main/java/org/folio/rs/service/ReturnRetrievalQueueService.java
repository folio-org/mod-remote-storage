package org.folio.rs.service;

import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.RetrievalQueueRecordUtils.buildReturnRetrievalQueueRecord;

import feign.FeignException;
import java.time.LocalDateTime;
import java.util.*;

import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.criteria.Predicate;

import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.AccessionFilterData;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.mapper.ReturnRetrievalQueueMapper;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.rs.util.RequestType;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class ReturnRetrievalQueueService {

  private static final String ID = "id";
  private static final String ITEM_BARCODE = "itemBarcode";
  private static final String RETRIEVED_DATE_TIME = "retrievedDateTime";
  private static final String REMOTE_STORAGE_ID = "remoteStorageId";
  private static final String REQUEST_DATE_TIME = "createdDateTime";
  private static final String HOLD_ID = "holdId";
  private static final String NOT_FOUND = " not found";
  private final ReturnRetrievalQueueRepository returnRetrievalQueueRepository;
  private final ReturnRetrievalQueueMapper returnRetrievalQueueMapper;
  private final LocationMappingsService locationMappingsService;
  private final InventoryClient inventoryClient;
  private final UsersClient usersClient;
  private final ServicePointsClient servicePointsClient;


  public RetrievalQueues getRetrievals(AccessionFilterData accessionFilterData) {
    log.debug("getRetrievals :: isPresented:{}",accessionFilterData.getIsPresented());
    var queueRecords = returnRetrievalQueueRepository.findAll(getCriteriaSpecification(accessionFilterData),
        new OffsetRequest(accessionFilterData.getOffset(), accessionFilterData.getLimit(), Sort.unsorted()));
    return returnRetrievalQueueMapper.mapEntitiesToRetrievalQueueCollection(queueRecords);
  }

  /**
   * This method returns last by creation time retrieval queue record searched by holdId and remoteStorageConfigurationId
   * @param holdId - request(hold) id
   * @param remoteStorageId - remote storage configuration id
   * @return retrieval queue record
   */
  public Optional<ReturnRetrievalQueueRecord> getLastRetrievalByHoldId(String holdId, String remoteStorageId) {
    log.debug("getLastRetrievalByHoldId :: holdId:{} and remoteStorageId:{}",holdId,remoteStorageId);
    return returnRetrievalQueueRepository.findAll(Specification.where(hasHoldId(holdId)
      .and(hasRemoteStorageId(remoteStorageId))), Sort.by(Sort.Direction.DESC, REQUEST_DATE_TIME)).stream().findFirst();
  }

  public void setRetrievedById(String retrievalQueueId) {
    log.debug("setRetrievedById :: retrievalQueueId{}",retrievalQueueId);
    Optional<ReturnRetrievalQueueRecord> retrievalQueue = returnRetrievalQueueRepository.findOne(Specification.where(hasId(retrievalQueueId).and(notRetrievedSpecification())));
    if (retrievalQueue.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with id " + retrievalQueueId + NOT_FOUND);
    }
    saveRetrievalQueueWithCurrentDate(retrievalQueue.get());
  }

  public void setRetrievedByBarcode(String barcode) {
    log.debug("setRetrievedByBarcode :: barcode:{}",barcode);
    List<ReturnRetrievalQueueRecord> retrievalQueueRecords = returnRetrievalQueueRepository.findAll(Specification.where(hasBarcode(barcode).and(notRetrievedSpecification())));
    if (retrievalQueueRecords.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with item barcode " + barcode + NOT_FOUND);
    }
    retrievalQueueRecords.forEach(this::saveRetrievalQueueWithCurrentDate);
  }

  public void processEventRequest(RequestEvent requestEvent) {
      log.info("processEventRequest :: Process moved request with id:{}",requestEvent.getHoldId());
      var item = getOriginalItemByBarcode(requestEvent);
      var locationMapping = getRemoteLocationConfigurationMapping(item);
      if (Objects.nonNull(locationMapping)) {
        log.info("Item location is remote, saving retrieval queue record");
        processEventRequest(requestEvent, item, locationMapping);
      }
  }

  private void processEventRequest(RequestEvent requestEvent, Item item, RemoteLocationConfigurationMapping locationMapping) {
    var returnRetrievalQueueRecord = buildReturnRetrievalQueueRecord(requestEvent, item,
        getUserByRequesterId(requestEvent), locationMapping, getPickupServicePoint(requestEvent.getPickupServicePointId()));
    log.info("Saving retrieval queue record with id {}", returnRetrievalQueueRecord.getId());
    returnRetrievalQueueRepository.save(returnRetrievalQueueRecord);
  }

  private PickupServicePoint getPickupServicePoint(String pickupServicePointId) {
    return servicePointsClient.getServicePoint(pickupServicePointId);
  }

  private Specification<ReturnRetrievalQueueRecord> getCriteriaSpecification(AccessionFilterData accessionFilterData) {
    return (rec, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(accessionFilterData.getIsPresented())) {
        predicates.add(builder.isNotNull(rec.get(RETRIEVED_DATE_TIME)));
      }
      if (Boolean.FALSE.equals(accessionFilterData.getIsPresented())) {
        predicates.add(builder.isNull(rec.get(RETRIEVED_DATE_TIME)));
      }
      if (Objects.nonNull(accessionFilterData.getRemoteStorageConfigurationId())) {
        predicates.add(builder.equal(rec.get(REMOTE_STORAGE_ID), stringToUUIDSafe(accessionFilterData.getRemoteStorageConfigurationId())));
      }
      if (Objects.nonNull(accessionFilterData.getCreateDate())) {
        predicates.add(builder.equal(rec.get(REQUEST_DATE_TIME), LocalDateTime.parse(accessionFilterData.getCreateDate())));
      }
      if (Objects.nonNull(accessionFilterData.getCreateDate())) {
        predicates.add(builder.equal(rec.get(REQUEST_DATE_TIME), LocalDateTime.parse(accessionFilterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveRetrievalQueueWithCurrentDate(ReturnRetrievalQueueRecord rec) {
    rec.setRetrievedDateTime(LocalDateTime.now());
    rec.setRequestType(RequestType.PYR.getType());
    returnRetrievalQueueRepository.save(rec);
  }

  private Specification<ReturnRetrievalQueueRecord> hasBarcode(String barcode) {
    return (rec, criteria, builder) -> builder.equal(rec.get(ITEM_BARCODE), barcode);
  }

  private Specification<ReturnRetrievalQueueRecord> notRetrievedSpecification() {
    return (rec, criteria, builder) -> builder.isNull(rec.get(RETRIEVED_DATE_TIME));
  }

  private Specification<ReturnRetrievalQueueRecord> hasId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(ID), stringToUUIDSafe(id));
  }

  private Specification<ReturnRetrievalQueueRecord> hasHoldId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(HOLD_ID), id);
  }

  private Specification<ReturnRetrievalQueueRecord> hasRemoteStorageId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(REMOTE_STORAGE_ID), stringToUUIDSafe(id));
  }

  private RemoteLocationConfigurationMapping getRemoteLocationConfigurationMapping(Item item) {
    return Objects.nonNull(item.getEffectiveLocation())
        ? locationMappingsService.getRemoteLocationConfigurationMapping(item.getEffectiveLocation().getId())
        : null;
  }

  private Item getOriginalItemByBarcode(RequestEvent requestEvent) {
    var item = inventoryClient.getItemByBarcodeOrNull(requestEvent.getItemBarCode());
    if (item == null) {
      throw new EntityNotFoundException("Item with barcode " + requestEvent.getItemBarCode() + NOT_FOUND);
    }
    return item;
  }

  private User getUserByRequesterId(RequestEvent requestEvent) {
    try {
      return usersClient.getUser(requestEvent.getRequesterId());
    } catch (FeignException.NotFound e) {
      throw new EntityNotFoundException("User with id " + requestEvent.getRequesterId() + NOT_FOUND);
    }
  }
}
