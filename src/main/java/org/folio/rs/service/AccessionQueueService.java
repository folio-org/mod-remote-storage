package org.folio.rs.service;

import static java.util.Optional.ofNullable;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.InstancesClient;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.domain.dto.Contributor;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class AccessionQueueService {

  private final AccessionQueueRepository accessionQueueRepository;
  private final LocationMappingsService locationMappingsService;
  private final InstancesClient instancesClient;
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
   * This method builds {@link AccessionQueueRecord} based on data from {@link Item} and
   * corresponding {@link Instance}
   *
   * @param item {@link Item} entity
   * @param instance {@link Instance} entity
   * @return accession queue record with populated data
   */
  private AccessionQueueRecord buildAccessionQueueRecord(Item item, Instance instance,
    LocationMapping locationMapping) {
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

  @SneakyThrows
  private String asJsonString(Object value) {
    return OBJECT_MAPPER.writeValueAsString(value);
  }

}
