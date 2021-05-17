package org.folio.rs.service;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.rs.util.ContributorType.AUTHOR;
import static org.folio.rs.util.IdentifierType.*;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;

import org.folio.rs.client.ContributorTypesClient;
import org.folio.rs.client.HoldingsStorageClient;
import org.folio.rs.client.IdentifierTypesClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.domain.dto.*;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.util.IdentifierType;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;

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
  private final InventoryClient inventoryClient;
  private final AccessionQueueMapper accessionQueueMapper;
  private final SecurityManagerService securityManagerService;
  private final FolioModuleMetadata moduleMetadata;
  private final HoldingsStorageClient holdingsStorageClient;
  private final IdentifierTypesClient identifierTypesClient;
  private final ContributorTypesClient contributorTypesClient;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  public void processAccessionQueueRecord(List<DomainEvent> events) {
    log.info("Starting processing events...");
    events.forEach(event -> {
      log.debug("Event received: {}", asJsonString(event));
      if (DomainEventType.UPDATE == event.getType() && isEffectiveLocationChanged(event)) {
        var item = event.getNewEntity();
        var systemUserParameters = securityManagerService.getSystemUserParameters(event.getTenant());
        FolioExecutionScopeExecutionContextManager.beginFolioExecutionContext(
          new AsyncFolioExecutionContext(systemUserParameters, moduleMetadata));
        var effectiveLocationId = item.getEffectiveLocationId();
        var locationMapping = locationMappingsService
          .getMappingByFinalLocationId(effectiveLocationId);
        if (nonNull(locationMapping)) {
          var instances = inventoryClient.getInstancesByQuery("id==" + item.getInstanceId());
          var instance = instances.getResult().get(0);
          var record = buildAccessionQueueRecord(item, instance, locationMapping);
          accessionQueueRepository.save(record);
          log.debug("Record prepared and saved: {}", record);
        } else {
          log.info("Location mapping with id={} not found. Accession queue record not created.", effectiveLocationId);
        }
      }
    });
  }

  public AccessionQueue processPostAccession(AccessionRequest accessionRequest) {
    var locationMapping = getLocationMapping(accessionRequest);
    var item = getItem(accessionRequest);
    var holdingsRecord = holdingsStorageClient.getHoldingsRecordsByQuery("id==" + item.getHoldingsRecordId()).getResult().get(0);
    var instance = inventoryClient.getInstancesByQuery("id==" + holdingsRecord.getInstanceId()).getResult().get(0);

    var accessionQueueRecord = buildAccessionQueueRecord(item, instance, locationMapping);
    accessionQueueRepository.save(accessionQueueRecord);

    item.setPermanentLocation(new ItemPermanentLocation().id(locationMapping.getFinalLocationId()));
    inventoryClient.putItem(item.getId(), item);

    if (isPermanentLocationsMismatch(holdingsRecord, item)) {
      if (isAllItemsInHoldingHaveSamePermanentLocation(item)) {
        changeHoldingsRecordPermanentLocation(holdingsRecord, item.getPermanentLocation().getId());
      } else {
        var holdingId = findHoldingWithSameRemoteLocation(holdingsRecord.getInstanceId(), item.getPermanentLocation().getId());
        if (isHoldingExists(holdingId)) {
          moveItemToHolding(item, holdingId);
        }
        // holding splitting will be implemented in scope of https://issues.folio.org/browse/MODRS-40
      }
    }
    return accessionQueueMapper.mapEntityToDto(accessionQueueRecord);
  }

  private boolean isPermanentLocationsMismatch(HoldingsRecord holdingsRecord, Item item) {
    return !item.getPermanentLocation().getId().equals(holdingsRecord.getPermanentLocationId());
  }

  private void changeHoldingsRecordPermanentLocation(HoldingsRecord holdingsRecord, String locationId) {
    holdingsRecord.setPermanentLocationId(locationId);
    holdingsStorageClient.putHoldingsRecord(holdingsRecord.getId(), holdingsRecord);
  }

  private String findHoldingWithSameRemoteLocation(String instanceId, String remoteLocationId) {
    return holdingsStorageClient.getHoldingsRecordsByQuery("instanceId==" + instanceId)
      .getResult().stream()
      .filter(h -> remoteLocationId.equals(h.getPermanentLocationId()))
      .map(HoldingsRecord::getId)
      .findFirst().orElse(null);
  }

  private boolean isHoldingExists(String holdingsRecordId) {
    return nonNull(holdingsRecordId);
  }

  private void moveItemToHolding(Item item, String holdingRecordId) {
    inventoryClient.moveItemsToHolding(new ItemsMove()
      .itemIds(Collections.singletonList(item.getId()))
      .toHoldingsRecordId(holdingRecordId));
  }

  private boolean isAllItemsInHoldingHaveSamePermanentLocation(Item item) {
    return inventoryClient.getItemsByQuery("holdingsRecordId==" + item.getHoldingsRecordId())
      .getResult()
      .stream()
      .allMatch(i -> nonNull(i.getPermanentLocation()) && item.getPermanentLocation().getId().equals(i.getPermanentLocation().getId()));
  }

  private LocationMapping getLocationMapping(AccessionRequest accessionRequest) {
    return locationMappingsService.getMappings(0, Integer.MAX_VALUE)
      .getMappings()
      .stream()
      .filter(lm -> accessionRequest.getRemoteStorageId().equals(lm.getRemoteConfigurationId()))
      .findFirst()
      .orElseThrow(() -> new EntityNotFoundException("No location was found for provided remote storage id"));
  }

  private Item getItem(AccessionRequest accessionRequest) {
    var items = inventoryClient.getItemsByQuery("barcode==" + accessionRequest.getItemBarcode());
    if (items.isEmpty()) {
      throw new EntityNotFoundException(String.format("Item with barcode=%s was not found", accessionRequest.getItemBarcode()));
    }
    return items.getResult().get(0);
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
    var isEffectiveLocationChanged = !Objects.equals(domainEvent.getOldEntity()
        .getEffectiveLocationId(),
      domainEvent.getNewEntity()
        .getEffectiveLocationId());
    log.info("isEffectiveLocationChanged: {}", isEffectiveLocationChanged);
    return isEffectiveLocationChanged;
  }

  /**
   * This method builds {@link AccessionQueueRecord} based on data from {@link Item} and corresponding {@link Instance}
   *
   * @param item     {@link Item} entity
   * @param instance {@link Instance} entity
   * @return accession queue record with populated data
   */
  private AccessionQueueRecord buildAccessionQueueRecord(Item item, Instance instance,
    LocationMapping locationMapping) {
    var publication = instance.getPublication().stream().findFirst();
    return AccessionQueueRecord.builder()
      .id(UUID.randomUUID())
      .itemBarcode(item.getBarcode())
      .createdDateTime(LocalDateTime.now())
      .accessionedDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(locationMapping.getRemoteConfigurationId()))
      .callNumber(ofNullable(item.getEffectiveCallNumberComponents())
        .map(ItemEffectiveCallNumberComponents::getCallNumber)
        .orElse(null))
      .instanceTitle(instance.getTitle())
      .instanceAuthor(extractAuthors(instance))
      .instanceContributors(extractContributors(instance))
      .publisher(publication
        .map(InstancePublication::getPublisher)
        .orElse(null))
      .publishYear(publication
        .map(InstancePublication::getDateOfPublication)
        .orElse(null))
      .publishPlace(publication
        .map(InstancePublication::getPlace)
        .orElse(null))
      .volume(item.getVolume())
      .enumeration(item.getEnumeration())
      .chronology(item.getChronology())
      .isbn(extractIdentifier(instance, ISBN))
      .issn(extractIdentifier(instance, ISSN))
      .oclc(extractIdentifier(instance, OCLC))
      .physicalDescription(String.join("; ", instance.getPhysicalDescriptions()))
      .materialType(ofNullable(item.getMaterialType()).map(ItemMaterialType::getName).orElse(null))
      .copyNumber(item.getCopyNumber())
      .permanentLocationId(UUID.fromString(locationMapping.getFinalLocationId()))
      .build();
  }

  private String extractAuthors(Instance instance) {
    var contributorTypeId = contributorTypesClient.getContributorTypesByQuery("name==" + AUTHOR)
      .getResult()
      .stream()
      .findFirst()
      .map(ContributorType::getId)
      .orElse(EMPTY);
    var authors = instance.getContributors().stream()
      .filter(c -> contributorTypeId.equals(c.getContributorTypeId()))
      .map(InstanceContributors::getName)
      .collect(Collectors.toList());
    return authors.isEmpty() ? extractContributors(instance) : String.join("; ", authors);
  }

  private String extractContributors(Instance instance) {
    return instance.getContributors()
      .stream()
      .map(InstanceContributors::getName)
      .collect(Collectors.joining("; "));
  }

  private String extractIdentifier(Instance instance, IdentifierType type) {
    var identifierTypeId = identifierTypesClient.getIdentifierTypesByQuery("name==" + type)
      .getResult()
      .stream()
      .findFirst()
      .map(org.folio.rs.domain.dto.IdentifierType::getId)
      .orElse(EMPTY);

    return instance.getIdentifiers().stream()
      .filter(i -> identifierTypeId.equals(i.getIdentifierTypeId()))
      .findFirst()
      .map(InstanceIdentifiers::getValue)
      .orElse(null); //NOSONAR
  }

  private Specification<AccessionQueueRecord> getCriteriaSpecification(FilterData filterData){
    return (record, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNotNull(record.get(ACCESSIONED_DATE_TIME)));
      }
      if (Boolean.FALSE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNull(record.get(ACCESSIONED_DATE_TIME)));
      }
      if (nonNull(filterData.getStorageId())) {
        predicates.add(builder.equal(record.get(REMOTE_STORAGE_ID), stringToUUIDSafe(filterData.getStorageId())));
      }
      if (nonNull(filterData.getCreateDate())) {
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
