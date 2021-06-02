package org.folio.rs.service;

import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.rs.domain.dto.AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION;
import static org.folio.rs.util.ContributorType.AUTHOR;
import static org.folio.rs.util.IdentifierType.ISBN;
import static org.folio.rs.util.IdentifierType.ISSN;
import static org.folio.rs.util.IdentifierType.OCLC;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityNotFoundException;
import javax.persistence.criteria.Predicate;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.ContributorTypesClient;
import org.folio.rs.client.HoldingsStorageClient;
import org.folio.rs.client.IdentifierTypesClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.domain.dto.AccessionQueue;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.AccessionRequest;
import org.folio.rs.domain.dto.ContributorType;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.HoldingsRecord;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.InstanceContributors;
import org.folio.rs.domain.dto.InstanceIdentifiers;
import org.folio.rs.domain.dto.InstancePublication;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.ItemMaterialType;
import org.folio.rs.domain.dto.ItemPermanentLocation;
import org.folio.rs.domain.dto.ItemsMove;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.error.AccessionException;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.util.IdentifierType;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
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
  private final InventoryClient inventoryClient;
  private final AccessionQueueMapper accessionQueueMapper;
  private final SecurityManagerService securityManagerService;
  private final FolioModuleMetadata moduleMetadata;
  private final HoldingsStorageClient holdingsStorageClient;
  private final IdentifierTypesClient identifierTypesClient;
  private final ContributorTypesClient contributorTypesClient;
  private final ConfigurationsService configurationsService;

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
          .getRemoteLocationConfigurationMapping(effectiveLocationId);
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
    var storageConfiguration = getStorageConfiguration(accessionRequest);
    var locationMapping = getLocationMapping(accessionRequest);
    var item = getItem(accessionRequest);
    var holdingsRecord = holdingsStorageClient.getHoldingsRecordsByQuery("id==" + item.getHoldingsRecordId()).getResult().get(0);
    var instance = inventoryClient.getInstancesByQuery("id==" + holdingsRecord.getInstanceId()).getResult().get(0);

    var remoteLocationId = locationMapping.getFolioLocationId();

    if (!holdingsRecordMatchLocation(holdingsRecord, remoteLocationId)) {
      if (shouldChangeHoldingsPermanentLocation(storageConfiguration, item, remoteLocationId)) {
        if (isChangePermanentLocationSetting(storageConfiguration)) {
          setLocationForItemsWithoutLocation(holdingsRecord);
        }
        changeHoldingsRecordPermanentLocation(holdingsRecord, remoteLocationId);
      } else {
        moveItemToHolding(item, findOrCreateHoldingWithSamePermanentLocation(holdingsRecord, remoteLocationId));
      }
    }

    changeItemPermanentLocation(item, remoteLocationId);
    var accessionQueueRecord = buildAccessionQueueRecord(item, instance, locationMapping);
    accessionQueueRepository.save(accessionQueueRecord);
    return accessionQueueMapper.mapEntityToDto(accessionQueueRecord);
  }

  private RemoteLocationConfigurationMapping getLocationMapping(AccessionRequest accessionRequest) {
    return locationMappingsService.getRemoteLocationConfigurationMappings(0, Integer.MAX_VALUE)
      .getMappings().stream()
      .filter(mapping -> accessionRequest.getRemoteStorageId().equals(mapping.getConfigurationId()))
      .findFirst()
      .orElseThrow(() -> new AccessionException(String.format("No location was found for remote storage id=%s", accessionRequest.getRemoteStorageId())));
  }

  private boolean shouldChangeHoldingsPermanentLocation(StorageConfiguration storageConfiguration, Item item, String location) {
    return isChangePermanentLocationSetting(storageConfiguration) || isAllItemsInHoldingHaveSamePermanentLocation(item, location);
  }

  private boolean isChangePermanentLocationSetting(StorageConfiguration storageConfiguration) {
    return CHANGE_PERMANENT_LOCATION == storageConfiguration.getAccessionWorkflowDetails();
  }

  private void setLocationForItemsWithoutLocation(HoldingsRecord holdingsRecord) {
    inventoryClient.getItemsByQuery("holdingsRecordId==" + holdingsRecord.getId())
      .getResult().forEach(i -> {
      if (isNull(i.getPermanentLocation()) && isNull(i.getTemporaryLocation())) {
        changeItemPermanentLocation(i, holdingsRecord.getPermanentLocationId());
      }
    });
  }

  private boolean holdingsRecordMatchLocation(HoldingsRecord holdingsRecord, String locationId) {
    return locationId.equals(holdingsRecord.getPermanentLocationId());
  }

  private void changeHoldingsRecordPermanentLocation(HoldingsRecord holdingsRecord, String locationId) {
    holdingsRecord.setPermanentLocationId(locationId);
    holdingsStorageClient.putHoldingsRecord(holdingsRecord.getId(), holdingsRecord);
  }

  private void changeItemPermanentLocation(Item item, String locationId) {
    item.setPermanentLocation(new ItemPermanentLocation().id(locationId));
    inventoryClient.putItem(item.getId(), item);
  }

  private String findOrCreateHoldingWithSamePermanentLocation(HoldingsRecord holdingsRecord, String remoteLocationId) {
    var holdings = holdingsStorageClient.getHoldingsRecordsByQuery("instanceId==" + holdingsRecord.getInstanceId())
      .getResult().stream()
      .filter(h -> remoteLocationId.equals(h.getPermanentLocationId()))
      .collect(Collectors.toList());

    if (holdings.isEmpty()) {
      return holdingsStorageClient.postHoldingsRecord(holdingsRecord
        .id(UUID.randomUUID().toString())
        .hrid(null)
        .permanentLocationId(remoteLocationId))
        .getId();
    } else if (holdings.size() == 1) {
      return holdings.get(0).getId();
    } else {
      throw new AccessionException("More than one holdings record exist with permanent location id=" + remoteLocationId);
    }
  }

  private void moveItemToHolding(Item item, String holdingRecordId) {
    inventoryClient.moveItemsToHolding(new ItemsMove()
      .itemIds(Collections.singletonList(item.getId()))
      .toHoldingsRecordId(holdingRecordId));
    item.setHoldingsRecordId(holdingRecordId);
  }

  private boolean isAllItemsInHoldingHaveSamePermanentLocation(Item item, String location) {
    return inventoryClient.getItemsByQuery("holdingsRecordId==" + item.getHoldingsRecordId())
      .getResult()
      .stream()
      .filter(i -> !item.getId().equals(i.getId()))
      .allMatch(i -> nonNull(i.getPermanentLocation()) && location.equals(i.getPermanentLocation().getId()));
  }

  private Item getItem(AccessionRequest accessionRequest) {
    var items = inventoryClient.getItemsByQuery("barcode==" + accessionRequest.getItemBarcode());
    if (items.isEmpty()) {
      throw new AccessionException(String.format("Item with barcode=%s was not found", accessionRequest.getItemBarcode()));
    }
    return items.getResult().get(0);
  }

  private StorageConfiguration getStorageConfiguration(AccessionRequest accessionRequest) {
    var storageConfiguration = configurationsService.getConfigurationById(accessionRequest.getRemoteStorageId());
    if (isNull(storageConfiguration)) {
      throw new AccessionException(
        String.format("No configuration was found for remote storage id=%s", accessionRequest.getRemoteStorageId()));
    } else if (isNull(storageConfiguration.getAccessionWorkflowDetails())) {
      throw new AccessionException("Remote storage configuration does not contain accession details");
    }
    return storageConfiguration;
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
    RemoteLocationConfigurationMapping locationMapping) {
    var publication = instance.getPublication().stream().findFirst();
    return AccessionQueueRecord.builder()
      .id(UUID.randomUUID())
      .itemBarcode(item.getBarcode())
      .createdDateTime(LocalDateTime.now())
      .accessionedDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(locationMapping.getConfigurationId()))
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
      .permanentLocationId(UUID.fromString(locationMapping.getFolioLocationId()))
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
    return (rec, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNotNull(rec.get(ACCESSIONED_DATE_TIME)));
      }
      if (Boolean.FALSE.equals(filterData.getIsPresented())) {
        predicates.add(builder.isNull(rec.get(ACCESSIONED_DATE_TIME)));
      }
      if (nonNull(filterData.getStorageId())) {
        predicates.add(builder.equal(rec.get(REMOTE_STORAGE_ID), stringToUUIDSafe(filterData.getStorageId())));
      }
      if (nonNull(filterData.getCreateDate())) {
        predicates.add(builder.equal(rec.get(CREATED_DATE_TIME), LocalDateTime.parse(filterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveAccessionQueueWithCurrentDate(AccessionQueueRecord record) {
    record.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(record);
  }

  private Specification<AccessionQueueRecord> hasBarcode(String barcode) {
    return (rec, criteria, builder) -> builder.equal(rec.get(ITEM_BARCODE), barcode);
  }

  private Specification<AccessionQueueRecord> notAccessioned() {
    return (rec, criteria, builder) -> builder.isNull(rec.get(ACCESSIONED_DATE_TIME));
  }

  private Specification<AccessionQueueRecord> hasId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(ID), stringToUUIDSafe(id));
  }

  @SneakyThrows
  private String asJsonString(Object value) {
    return OBJECT_MAPPER.writeValueAsString(value);
  }
}
