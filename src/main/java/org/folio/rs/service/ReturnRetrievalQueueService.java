package org.folio.rs.service;

import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.RetrievalQueueRecordUtils.buildRetrievalQueueRecord;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;

import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;

import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.PickupServicePoint;
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


  public RetrievalQueues getRetrievals(FilterData filterData) {
    var queueRecords = returnRetrievalQueueRepository.findAll(getCriteriaSpecification(filterData),
        new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return returnRetrievalQueueMapper.mapEntitiesToRetrievalQueueCollection(queueRecords);
  }

  /**
   * This method returns last by creation time retrieval queue record searched by holdId and remoteStorageConfigurationId
   * @param holdId - request(hold) id
   * @param remoteStorageId - remote storage configuration id
   * @return retrieval queue record
   */
  public Optional<ReturnRetrievalQueueRecord> getLastRetrievalByHoldId(String holdId, String remoteStorageId) {
    return returnRetrievalQueueRepository.findAll(Specification.where(hasHoldId(holdId)
      .and(hasRemoteStorageId(remoteStorageId))), Sort.by(Sort.Direction.DESC, REQUEST_DATE_TIME)).stream().findFirst();
  }

  public void setRetrievedById(String retrievalQueueId) {
    Optional<ReturnRetrievalQueueRecord> retrievalQueue = returnRetrievalQueueRepository.findOne(Specification.where(hasId(retrievalQueueId).and(notRetrievedSpecification())));
    if (retrievalQueue.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with id " + retrievalQueueId + NOT_FOUND);
    }
    saveRetrievalQueueWithCurrentDate(retrievalQueue.get());
  }

  public void setRetrievedByBarcode(String barcode) {
    Optional<ReturnRetrievalQueueRecord> retrievalQueueRecord = returnRetrievalQueueRepository.findOne(Specification.where(hasBarcode(barcode).and(notRetrievedSpecification())));
    if (retrievalQueueRecord.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with item barcode " + barcode + NOT_FOUND);
    }
    saveRetrievalQueueWithCurrentDate(retrievalQueueRecord.get());
  }

  public void processEventRequest(RequestEvent requestEvent) {
      log.info("Process moved request with id " + requestEvent.getHoldId());
      Item item = getOriginalItemByBarcode(requestEvent);
      LocationMapping locationMapping = getLocationMapping(item);
      if (Objects.nonNull(locationMapping)) {
        log.info("Item location is remote, saving retrieval queue record");
        processEventRequest(requestEvent, item, locationMapping);
      }
  }

  private void processEventRequest(RequestEvent requestEvent, Item item, LocationMapping locationMapping) {
    ReturnRetrievalQueueRecord record = buildRetrievalQueueRecord(requestEvent, item,
        getUserByRequesterId(requestEvent), locationMapping, getPickupServicePoint(requestEvent.getPickupServicePointId()));
    log.info("Saving retrieval queue record with id {}", record.getId());
    returnRetrievalQueueRepository.save(record);
  }

  private PickupServicePoint getPickupServicePoint(String pickupServicePointId) {
    return servicePointsClient.getServicePoint(pickupServicePointId);
  }

  private Specification<ReturnRetrievalQueueRecord> getCriteriaSpecification(FilterData filterData) {
    return (record, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNotNull(record.get(RETRIEVED_DATE_TIME)));
      }
      if (Boolean.FALSE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNull(record.get(RETRIEVED_DATE_TIME)));
      }
      if (Objects.nonNull(filterData.getStorageId())) {
        predicates.add(builder.equal(record.get(REMOTE_STORAGE_ID), stringToUUIDSafe(filterData.getStorageId())));
      }
      if (Objects.nonNull(filterData.getCreateDate())) {
        predicates.add(builder.equal(record.get(REQUEST_DATE_TIME), LocalDateTime.parse(filterData.getCreateDate())));
      }
      if (Objects.nonNull(filterData.getCreateDate())) {
        predicates.add(builder.equal(record.get(REQUEST_DATE_TIME), LocalDateTime.parse(filterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveRetrievalQueueWithCurrentDate(ReturnRetrievalQueueRecord record) {
    record.setRetrievedDateTime(LocalDateTime.now());
    record.setRequestType(RequestType.PYR.getType());
    returnRetrievalQueueRepository.save(record);
  }

  private Specification<ReturnRetrievalQueueRecord> hasBarcode(String barcode) {
    return (record, criteria, builder) -> builder.equal(record.get(ITEM_BARCODE), barcode);
  }

  private Specification<ReturnRetrievalQueueRecord> notRetrievedSpecification() {
    return (record, criteria, builder) -> builder.isNull(record.get(RETRIEVED_DATE_TIME));
  }

  private Specification<ReturnRetrievalQueueRecord> hasId(String id) {
    return (record, criteria, builder) -> builder.equal(record.get(ID), stringToUUIDSafe(id));
  }

  private Specification<ReturnRetrievalQueueRecord> hasHoldId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(HOLD_ID), id);
  }

  private Specification<ReturnRetrievalQueueRecord> hasRemoteStorageId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(REMOTE_STORAGE_ID), stringToUUIDSafe(id));
  }

  private LocationMapping getLocationMapping(Item item) {
    return Objects.nonNull(item.getEffectiveLocation())
        ? locationMappingsService.getMappingByFolioLocationId(item.getEffectiveLocation().getId())
        : null;
  }

  private Item getOriginalItemByBarcode(RequestEvent requestEvent) {
    ResultList<Item> items = inventoryClient.getItemsByQuery("barcode==" + requestEvent.getItemBarCode());
    if (isEmpty(items.getResult())) {
      throw new EntityNotFoundException("Item with barcode " + requestEvent.getItemBarCode() + NOT_FOUND);
    }
    return items.getResult().get(0);
  }

  private User getUserByRequesterId(RequestEvent requestEvent) {
    ResultList<User> users = usersClient.getUsersByQuery("id==" + requestEvent.getRequesterId());
    if (isEmpty(users.getResult())) {
      throw new EntityNotFoundException("User with id " + requestEvent.getRequesterId() + NOT_FOUND);
    }
    return users.getResult().get(0);
  }
}
