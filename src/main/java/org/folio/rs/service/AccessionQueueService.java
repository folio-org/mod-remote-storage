package org.folio.rs.service;

import static java.util.Optional.ofNullable;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.InstancesClient;
import org.folio.rs.domain.dto.Contributor;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.DomainEvent;
import org.folio.rs.dto.EffectiveCallNumberComponents;
import org.folio.rs.dto.Item;
import org.folio.rs.repository.AccessionQueueRepository;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class AccessionQueueService {

  private final AccessionQueueRepository accessionQueueRepository;
  private final LocationMappingsService locationMappingsService;
  private final InstancesClient instancesClient;

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

}
