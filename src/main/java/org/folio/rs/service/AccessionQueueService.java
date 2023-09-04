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
import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.criteria.Predicate;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.ContributorTypesClient;
import org.folio.rs.client.HoldingsStorageClient;
import org.folio.rs.client.IdentifierTypesClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ItemNoteTypesClient;
import org.folio.rs.domain.dto.AccessionQueue;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.AccessionRequest;
import org.folio.rs.domain.dto.ContributorType;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.domain.dto.AccessionFilterData;
import org.folio.rs.domain.dto.HoldingsRecord;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.InstanceContributors;
import org.folio.rs.domain.dto.InstancePublication;
import org.folio.rs.domain.dto.InstanceIdentifiers;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.ItemMaterialType;
import org.folio.rs.domain.dto.ItemNote;
import org.folio.rs.domain.dto.ItemPermanentLocation;
import org.folio.rs.domain.dto.ItemsMove;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.AccessionQueueRecord_;
import org.folio.rs.domain.entity.ItemNoteEntity;
import org.folio.rs.error.AccessionException;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.util.IdentifierType;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class AccessionQueueService {

  private final AccessionQueueRepository accessionQueueRepository;
  private final LocationMappingsService locationMappingsService;
  private final InventoryClient inventoryClient;
  private final AccessionQueueMapper accessionQueueMapper;
  private final FolioModuleMetadata moduleMetadata;
  private final HoldingsStorageClient holdingsStorageClient;
  private final IdentifierTypesClient identifierTypesClient;
  private final ContributorTypesClient contributorTypesClient;
  private final ConfigurationsService configurationsService;
  private final ItemNoteTypesClient itemNoteTypesClient;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;

  public void processAccessionQueueRecord(List<DomainEvent> events) {
    log.info("Starting processing events...");
    events.forEach(event -> {
      log.info("Event received: {}", asJsonString(event));
      if (DomainEventType.CREATE == event.getType() ||
          DomainEventType.UPDATE == event.getType() && isEffectiveLocationChanged(event)) {
        var item = event.getNewEntity();
        systemUserScopedExecutionService.executeAsyncSystemUserScoped(event.getTenant(), () -> {
          log.info("processAccessionQueueRecord:: Executing inside executeAsyncSystemUserScope");
          var effectiveLocationId = item.getEffectiveLocationId();
          var locationMapping = locationMappingsService
            .getRemoteLocationConfigurationMapping(effectiveLocationId);
          if (nonNull(locationMapping)) {
            var instances = inventoryClient.getInstancesByQuery("id==" + item.getInstanceId());
            var instance = instances.getResult().get(0);
            var record = buildAccessionQueueRecord(item, instance, locationMapping);
            accessionQueueRepository.save(record);
            log.info("Record prepared and saved for item barcode: {}", record.getItemBarcode());
          } else {
            log.info("Location mapping with id={} not found. Accession queue record not created.", effectiveLocationId);
          }
        });
      }
    });
  }


  public AccessionQueue processPostAccession(AccessionRequest accessionRequest) {
    log.debug("processPostAccession :: itemBarcode:{} remoteStorageId:{}",accessionRequest.getItemBarcode(),
      accessionRequest.getRemoteStorageId());
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

    item = inventoryClient.getItemByBarcode(item.getBarcode());
    changeItemPermanentLocation(item, remoteLocationId);
    var accessionQueueRecord = buildAccessionQueueRecord(item, instance, locationMapping);
    accessionQueueRecord.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(accessionQueueRecord);
    return accessionQueueMapper.mapEntityToDto(accessionQueueRecord);
  }

  private RemoteLocationConfigurationMapping getLocationMapping(AccessionRequest accessionRequest) {
    return locationMappingsService.getRemoteLocationConfigurationMappings(LocationMappingFilterData.builder().build())
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

  public AccessionQueues getAccessions(AccessionFilterData accessionFilterData) {
    var queueRecords = accessionQueueRepository.findAll(getCriteriaSpecification(accessionFilterData),
        new OffsetRequest(accessionFilterData.getOffset(), accessionFilterData.getLimit(), Sort.unsorted()));
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
    List<AccessionQueueRecord> accessionQueues = accessionQueueRepository.findAll(Specification.where(hasBarcode(barcode).and(notAccessioned())));
    if (accessionQueues.isEmpty()) {
      throw new EntityNotFoundException("Accession queue with item barcode " + barcode + " not found");
    }
    accessionQueues.forEach(this::saveAccessionQueueWithCurrentDate);
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
    var itemNotes = ofNullable(item.getNotes())
      .orElseGet(Collections::emptyList)
      .stream()
      .map(this::buildItemNote)
      .collect(Collectors.toList());
    return AccessionQueueRecord.builder()
      .id(UUID.randomUUID())
      .itemBarcode(item.getBarcode())
      .createdDateTime(LocalDateTime.now())
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
      .notes(itemNotes)
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

  private Specification<AccessionQueueRecord> getCriteriaSpecification(AccessionFilterData accessionFilterData){
    return (rec, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      if (Boolean.TRUE.equals(accessionFilterData.getIsPresented())) {
        predicates.add(builder.isNotNull(rec.get(AccessionQueueRecord_.accessionedDateTime)));
      }
      if (Boolean.FALSE.equals(accessionFilterData.getIsPresented())) {
        predicates.add(builder.isNull(rec.get(AccessionQueueRecord_.accessionedDateTime)));
      }
      if (nonNull(accessionFilterData.getRemoteStorageConfigurationId())) {
        predicates.add(builder.equal(rec.get(AccessionQueueRecord_.remoteStorageId), stringToUUIDSafe(accessionFilterData.getRemoteStorageConfigurationId())));
      }
      if (nonNull(accessionFilterData.getCreateDate())) {
        predicates.add(builder.equal(rec.get(AccessionQueueRecord_.createdDateTime), LocalDateTime.parse(accessionFilterData.getCreateDate())));
      }
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private void saveAccessionQueueWithCurrentDate(AccessionQueueRecord record) {
    record.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(record);
  }

  private Specification<AccessionQueueRecord> hasBarcode(String barcode) {
    return (rec, criteria, builder) -> builder.equal(rec.get(AccessionQueueRecord_.itemBarcode), barcode);
  }

  private Specification<AccessionQueueRecord> notAccessioned() {
    return (rec, criteria, builder) -> builder.isNull(rec.get(AccessionQueueRecord_.accessionedDateTime));
  }

  private Specification<AccessionQueueRecord> hasId(String id) {
    return (rec, criteria, builder) -> builder.equal(rec.get(AccessionQueueRecord_.id), stringToUUIDSafe(id));
  }

  @SneakyThrows
  private String asJsonString(Object value) {
    return OBJECT_MAPPER.writeValueAsString(value);
  }

  private ItemNoteEntity buildItemNote(ItemNote itemNote) {
    var noteType = itemNoteTypesClient.getItemNoteTypeById(itemNote.getItemNoteTypeId()).getName();
    return ItemNoteEntity.builder()
      .noteType(noteType)
      .note(itemNote.getNote())
      .staffOnly(itemNote.getStaffOnly())
      .build();
  }
}
