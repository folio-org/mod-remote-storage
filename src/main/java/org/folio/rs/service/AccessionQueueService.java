package org.folio.rs.service;

import static java.util.Optional.ofNullable;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.InstancesClient;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.domain.dto.Contributor;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.DomainEvent;
import org.folio.rs.dto.EffectiveCallNumberComponents;
import org.folio.rs.dto.Item;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.domain.PageRequest;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Optional;
import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class AccessionQueueService {

  private static final String ID = "id";
  private static final String ITEM_BARCODE = "itemBarcode";
  private static final String ACCESSIONED_DATE_TIME = "accessionedDateTime";
  private static final String REMOTE_STORAGE_ID = "remoteStorageId";
  private static final String CREATED_DATE_TIME = "createdDateTime";
  private final AccessionQueueRepository accessionQueueRepository;
  private final LocationMappingsService locationMappingsService;
  private final InstancesClient instancesClient;
  private final AccessionQueueMapper accessionQueueMapper;
  private final SecurityManagerService securityManagerService;
  private final FolioModuleMetadata moduleMetadata;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  public void processAccessionQueueRecord(List<DomainEvent> events) {
    log.info("Starting processing events...");
    events.forEach(event -> {
      log.info("Event received: {}", asJsonString(event));
      if (isEffectiveLocationChanged(event)) {
        var item = event.getNewEntity();
        var systemUserParameters = securityManagerService.getSystemUserParameters(event.getTenant());
        FolioExecutionScopeExecutionContextManager.beginFolioExecutionContext(
      new AsyncFolioExecutionContext(systemUserParameters, moduleMetadata));
        var locationMapping = locationMappingsService
          .getMappingByFolioLocationId(item.getEffectiveLocationId());
        if (Objects.nonNull(locationMapping)) {
          var instances = instancesClient.query("id==" + item.getInstanceId());
          var instance = instances.getResult().get(0);
          var record = buildAccessionQueueRecord(item, instance, locationMapping);
          accessionQueueRepository.save(record);
          log.info("Record prepared and saved: {}", record);
        }
      }
    });
  }

  public AccessionQueues getAccessions(FilterData filterData) {
    var queueRecords = accessionQueueRepository.findAll(getCriteriaSpecification(filterData),
        new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return accessionQueueMapper.mapEntitiesToAccessionQueueCollection(queueRecords);
  }

  public void setAccessionedById(String accessionQueueId) {
    Optional<AccessionQueueRecord> accessionQueue = accessionQueueRepository.findOne(Specification.where(hasId(accessionQueueId).and(notAccessioned())));
    if (accessionQueue.isPresent()) {
      saveAccessionQueueWithCurrentDate(accessionQueue.get());
    } else {
      throw new EntityNotFoundException("Accession queue with id " + accessionQueueId + " not found");
    }
  }

  public void setAccessionedByBarcode(String barcode) {
    Optional<AccessionQueueRecord> accessionQueue = accessionQueueRepository.findOne(Specification.where(hasBarcode(barcode).and(notAccessioned())));
    if (accessionQueue.isPresent()) {
      saveAccessionQueueWithCurrentDate(accessionQueue.get());
    } else {
      throw new EntityNotFoundException("Accession queue with item barcode " + barcode + " not found");
    }
  }

  /**
   * This method determines if effective location is changed
   *
   * @param domainEvent {@link DomainEvent<Item>} with original and updated {@link Item}
   * @return true if effective location is changed, otherwise - false
   */
  private boolean isEffectiveLocationChanged(DomainEvent domainEvent) {
    return !Objects.equals(domainEvent.getOldEntity()
        .getEffectiveLocationId(),
      domainEvent.getNewEntity()
        .getEffectiveLocationId());
  }

  /**
   * This method builds {@link AccessionQueueRecord} based on data from {@link Item} and corresponding {@link Instance}
   *
   * @param item     {@link Item} entity
   * @param instance {@link Instance} entity
   * @return accession queue record with populated data
   */
  private AccessionQueueRecord buildAccessionQueueRecord(Item item, Instance instance, LocationMapping locationMapping) {
    return AccessionQueueRecord.builder()
      .id(UUID.randomUUID())
      .itemBarcode(item.getBarcode())
      .createdDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(locationMapping.getConfigurationId()))
      .callNumber(ofNullable(item.getEffectiveCallNumberComponents())
        .map(EffectiveCallNumberComponents::getCallNumber)
        .orElse(null))
      .instanceTitle(instance.getTitle())
      .instanceAuthor(instance.getContributors()
        .stream()
        .map(Contributor::getName)
        .collect(Collectors.joining("; ")))
      .build();
  }

  private Specification<AccessionQueueRecord> getCriteriaSpecification(FilterData filterData){
    return (record, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(filterData.getAccessioned())) {
        predicates.add(builder.isNotNull(record.get(ACCESSIONED_DATE_TIME)));
      }
      if (Objects.nonNull(filterData.getStorageId())) {
        predicates.add(builder.equal(record.get(REMOTE_STORAGE_ID), stringToUUIDSafe(filterData.getStorageId())));
      }
      if (Objects.nonNull(filterData.getCreateDate())) {
        predicates.add(builder.equal(record.get(CREATED_DATE_TIME), LocalDateTime.parse(filterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveAccessionQueueWithCurrentDate(AccessionQueueRecord record) {
    record.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(record);
  }

  private Specification<AccessionQueueRecord> hasBarcode(String barcode) {
    return (record, criteria, builder) -> builder.equal(record.get(ITEM_BARCODE), barcode);
  }

  private Specification<AccessionQueueRecord> notAccessioned() {
    return (record, criteria, builder) -> builder.isNull(record.get(ACCESSIONED_DATE_TIME));
  }

  private Specification<AccessionQueueRecord> hasId(String id) {
    return (record, criteria, builder) -> builder.equal(record.get(ID), stringToUUIDSafe(id));
  }

  @SneakyThrows
  private String asJsonString(Object value) {
    return OBJECT_MAPPER.writeValueAsString(value);
  }
}
