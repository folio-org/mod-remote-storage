package org.folio.rs.service;

import static java.util.Optional.ofNullable;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.InstancesClient;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.Contributor;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.DomainEvent;
import org.folio.rs.dto.EffectiveCallNumberComponents;
import org.folio.rs.dto.Item;
import org.folio.rs.mapper.AccessionQueueMapper;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class AccessionQueueService {

  private final AccessionQueueRepository accessionQueueRepository;
  private final LocationMappingsService locationMappingsService;
  private final InstancesClient instancesClient;
  private final AccessionQueueMapper accessionQueueMapper;

  public void processAccessionQueueRecord(List<DomainEvent> events) {

    events.forEach(event -> {

      if (isEffectiveLocationChanged(event)) {
        var item = event.getNewEntity();

        var locationMapping = locationMappingsService.getMappingByFolioLocationId(item.getEffectiveLocationId());

        if (Objects.nonNull(locationMapping)) {
          var instances = instancesClient.query("id==" + item.getInstanceId());
          var instance = instances.getResult().get(0);
          var record = buildAccessionQueueRecord(item, instance, locationMapping);
          accessionQueueRepository.save(record);
        }
      }
    });
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
      .callNumber(ofNullable(item.getEffectiveCallNumberComponents()).map(EffectiveCallNumberComponents::getCallNumber)
        .orElse(null))
      .instanceTitle(instance.getTitle())
      .instanceAuthor(instance.getContributors()
        .stream()
        .map(Contributor::getName)
        .collect(Collectors.joining("; ")))
      .build();
  }

  public AccessionQueues getAccessions(FilterData filterData) {
    AccessionQueueRecord queueRecord = getAccessionQueueSearchModel(filterData);
    var queueRecords = accessionQueueRepository.findAll(Example.of(queueRecord),
        new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return accessionQueueMapper.mapEntitiesToAccessionQueueCollection(queueRecords);
  }

  private AccessionQueueRecord getAccessionQueueSearchModel(FilterData filterData) {
    AccessionQueueRecord queueRecord = new AccessionQueueRecord();
    if (Objects.nonNull(filterData.getAccessioned())) {
      queueRecord.setAccessioned(filterData.getAccessioned());
    }
    if (Objects.nonNull(filterData.getStorageId())) {
      queueRecord.setRemoteStorageId(stringToUUIDSafe(filterData.getStorageId()));
    }
    if (Objects.nonNull(filterData.getCreateDate())) {
      queueRecord.setCreatedDateTime(LocalDateTime.parse(filterData.getCreateDate()));
    }
    return queueRecord;
  }

  public void setAccessioned(String accessionQueueId) {
    Optional<AccessionQueueRecord> accessionQueue= findAccessionQueueById(accessionQueueId);
    if (accessionQueue.isPresent()) {
      saveAccessionQueueWithCurrentDate(accessionQueue.get());
    } else {
      throw new EntityNotFoundException("Accession queue with id " + accessionQueueId + " not found");
    }
  }

  private void saveAccessionQueueWithCurrentDate(AccessionQueueRecord record) {
    record.setAccessionedDateTime(LocalDateTime.now());
    record.setAccessioned(true);
    accessionQueueRepository.save(record);
  }

  private Optional<AccessionQueueRecord> findAccessionQueueById(String accessionQueueId) {
    AccessionQueueRecord queueRecord = new AccessionQueueRecord();
    queueRecord.setId(stringToUUIDSafe(accessionQueueId));
    return accessionQueueRepository.findOne(Example.of(queueRecord));
  }
}
