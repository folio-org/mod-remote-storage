package org.folio.rs.service;

import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;

import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.EventRequest;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemContributorNames;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.PlainMapping;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.mapper.RetrievalQueueMapper;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class RetrievalQueueService {

  private static final String ID = "id";
  private static final String ITEM_BARCODE = "itemBarcode";
  private static final String RETRIEVED_DATE_TIME = "retrievedDateTime";
  private static final String REMOTE_STORAGE_ID = "remoteStorageId";
  private static final String REQUEST_DATE_TIME = "createdDateTime";
  private static final String HOLD_ID = "holdId";
  private static final String NOT_FOUND = " not found";
  private static final String REQUEST_TYPE_DEFAULT = "PYR";
  private final RetrievalQueueRepository retrievalQueueRepository;
  private final RetrievalQueueMapper retrievalQueueMapper;
  private final LocationMappingsService locationMappingsService;
  private final InventoryClient inventoryClient;
  private final UsersClient usersClient;
  private final ServicePointsClient servicePointsClient;


  public RetrievalQueues getRetrievals(FilterData filterData) {
    var queueRecords = retrievalQueueRepository.findAll(getCriteriaSpecification(filterData),
        new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return retrievalQueueMapper.mapEntitiesToRetrievalQueueCollection(queueRecords);
  }

  /**
   * This method returns last by creation time retrieval queue record searched by holdId and remoteStorageConfigurationId
   * @param holdId - request(hold) id
   * @param remoteStorageId - remote storage configuration id
   * @return retrieval queue record
   */
  public Optional<RetrievalQueueRecord> getLastRetrievalByHoldId(String holdId, String remoteStorageId) {
    return retrievalQueueRepository.findAll(Specification.where(hasHoldId(holdId)
      .and(hasRemoteStorageId(remoteStorageId))), Sort.by(Sort.Direction.DESC, REQUEST_DATE_TIME)).stream().findFirst();
  }

  public void setRetrievedById(String retrievalQueueId) {
    Optional<RetrievalQueueRecord> retrievalQueue = retrievalQueueRepository.findOne(Specification.where(hasId(retrievalQueueId).and(notRetrievedSpecification())));
    if (retrievalQueue.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with id " + retrievalQueueId + NOT_FOUND);
    }
    saveRetrievalQueueWithCurrentDate(retrievalQueue.get());
  }

  public void setRetrievedByBarcode(String barcode) {
    Optional<RetrievalQueueRecord> retrievalQueueRecord = retrievalQueueRepository.findOne(Specification.where(hasBarcode(barcode).and(notRetrievedSpecification())));
    if (retrievalQueueRecord.isEmpty()) {
      throw new EntityNotFoundException("Retrieval queue record with item barcode " + barcode + NOT_FOUND);
    }
    saveRetrievalQueueWithCurrentDate(retrievalQueueRecord.get());
  }

  public void processEventRequest(EventRequest eventRequest) {
      log.info("Process moved request with id " + eventRequest.getHoldId());
      Item item = getOriginalItemByBarcode(eventRequest);
      var locationMapping = getLocationMapping(item);
      if (Objects.nonNull(locationMapping)) {
        log.info("Item location is remote, saving retrieval queue record");
        processEventRequest(eventRequest, item, locationMapping);
      }

  }

  private void processEventRequest(EventRequest eventRequest, Item item, PlainMapping locationMapping) {
    RetrievalQueueRecord record = buildRetrievalQueueRecord(eventRequest, item,
        getUserByRequesterId(eventRequest), locationMapping, getPickupServicePoint(eventRequest.getPickupServicePointId()));
    log.info("Saving retrieval queue record with id {}", record.getId());
    retrievalQueueRepository.save(record);
  }

  private PickupServicePoint getPickupServicePoint(String pickupServicePointId) {
    return servicePointsClient.getServicePoint(pickupServicePointId);
  }

  private Specification<RetrievalQueueRecord> getCriteriaSpecification(FilterData filterData) {
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

  private void saveRetrievalQueueWithCurrentDate(RetrievalQueueRecord record) {
    record.setRetrievedDateTime(LocalDateTime.now());
    record.setRequestType(REQUEST_TYPE_DEFAULT);
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

  private Specification<RetrievalQueueRecord> hasHoldId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(HOLD_ID), id);
  }

  private Specification<RetrievalQueueRecord> hasRemoteStorageId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(REMOTE_STORAGE_ID), stringToUUIDSafe(id));
  }

  private PlainMapping getLocationMapping(Item item) {
    return Objects.nonNull(item.getEffectiveLocation())
        ? locationMappingsService.getPlainMapping(item.getEffectiveLocation().getId())
        : null;
  }

  private Item getOriginalItemByBarcode(EventRequest eventRequest) {
    ResultList<Item> items = inventoryClient.getItemsByQuery("barcode==" + eventRequest.getItemBarCode());
    if (isEmpty(items.getResult())) {
      throw new EntityNotFoundException("Item with barcode " + eventRequest.getItemBarCode() + NOT_FOUND);
    }
    return items.getResult().get(0);
  }

  private User getUserByRequesterId(EventRequest eventRequest) {
    ResultList<User> users = usersClient.getUsersByQuery("id==" + eventRequest.getRequesterId());
    if (isEmpty(users.getResult())) {
      throw new EntityNotFoundException("User with id " + eventRequest.getRequesterId() + NOT_FOUND);
    }
    return users.getResult().get(0);
  }

  private RetrievalQueueRecord buildRetrievalQueueRecord(EventRequest eventRequest,
    Item item, User patron, PlainMapping mapping, PickupServicePoint pickupServicePoint) {
    return RetrievalQueueRecord.builder()
        .id(UUID.randomUUID())
        .holdId(eventRequest.getHoldId())
        .patronBarcode(patron.getBarcode())
        .patronName(patron.getUsername())
        .callNumber(getCallNumber(item))
        .itemBarcode(eventRequest.getItemBarCode())
        .createdDateTime(LocalDateTime.now())
        .pickupLocation(pickupServicePoint.getCode())
        .requestStatus(eventRequest.getRequestStatus())
        .requestNote(eventRequest.getRequestNote())
        .remoteStorageId(stringToUUIDSafe(mapping.getConfigurationId()))
        .instanceTitle(item.getTitle())
        .instanceAuthor(getContributorNames(item))
        .requestType(REQUEST_TYPE_DEFAULT)
        .build();
  }

  private String getContributorNames(Item item) {
    return isEmpty(item.getContributorNames())
        ? null
        : item.getContributorNames().stream()
            .map(ItemContributorNames::getName)
            .collect(Collectors.joining("; "));
  }

  private String getCallNumber(Item item) {
    ItemEffectiveCallNumberComponents components = item.getEffectiveCallNumberComponents();
    return Objects.nonNull(components) ? components.getCallNumber() : null;
  }
}
