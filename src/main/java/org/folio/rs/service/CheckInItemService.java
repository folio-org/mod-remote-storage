package org.folio.rs.service;

import static java.lang.String.format;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.folio.rs.error.CheckInException;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;


@Service
@Log4j2
@RequiredArgsConstructor
public class CheckInItemService {

  private final CirculationClient circulationClient;
  private final ExtendedRemoteLocationConfigurationMappingsRepository mappingsRepository;
  private final LocationClient locationClient;
  private final ReturnRetrievalQueueService returnRetrievalQueueService;
  private final InventoryClient inventoryClient;

  public void checkInItemByBarcode(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.debug("checkInItemByBarcode :: Start check-in process for item with barcode:{} and remoteStorageConfigurationId:{}",
      checkInItem.getItemBarcode(),remoteStorageConfigurationId);
    var remoteLocations = getRemoteLocationsByRemoteStorageConfigurationId(remoteStorageConfigurationId);
    var checkInLocation = getCheckInLocationId(remoteLocations, checkInItem);
    circulationClient.checkIn(CheckInCirculationRequest.of(checkInItem.getItemBarcode(),
      getPrimaryServicePoint(checkInLocation), Instant.now().truncatedTo(ChronoUnit.MILLIS)));
    log.info("checkInItemByBarcode :: Check-in success for item with barcode:{} ",checkInItem.getItemBarcode());
  }

  public void checkInItemByHoldId(String remoteStorageConfigurationId, CheckInItemByHoldId checkInItemByHoldId) {
    var holdId = checkInItemByHoldId.getHoldId();
    log.debug("checkInItemByHoldId :: Start check-in process for item with associated request with id:{}",holdId);
    var retrievalQueueRecord = returnRetrievalQueueService.getLastRetrievalByHoldId(holdId, remoteStorageConfigurationId);
    var barcode = retrievalQueueRecord
      .orElseThrow(() -> new CheckInException(
        format("Retrieval Queue Record with holdId=%s not found for remoteStorageId=%s", holdId, remoteStorageConfigurationId)))
      .getItemBarcode();
    var checkInItemByBarcode = new CheckInItem();
    checkInItemByBarcode.setItemBarcode(barcode);
    checkInItemByBarcode(remoteStorageConfigurationId, checkInItemByBarcode);
    log.info("checkInItemByHoldId :: checkInItemByBarcode:{}"+checkInItemByBarcode.getItemBarcode());
  }

  private List<String> getRemoteLocationsByRemoteStorageConfigurationId(String remoteStorageConfigurationId) {
    var locations = mappingsRepository.findAllByRemoteStorageConfigurationId(UUID.fromString(remoteStorageConfigurationId)).stream()
      .map(RemoteLocationConfigurationMappingEntity::getFinalLocationId)
      .map(UUID::toString)
      .collect(Collectors.toList());
    if (locations.isEmpty()) {
      throw new CheckInException("Folio location does not exist for remoteStorageConfigurationId " + remoteStorageConfigurationId);
    }
    return locations;
  }

  private String getCheckInLocationId(List<String> remoteLocations, CheckInItem checkInItem) {
    if (remoteLocations.size() == 1) {
      return remoteLocations.get(0);
    }
    var item = inventoryClient.getItemByBarcode(checkInItem.getItemBarcode());
    if (remoteLocations.contains(item.getEffectiveLocation().getId())) {
      return item.getEffectiveLocation().getId();
    }
    log.info("Item with barcode=" + item.getBarcode() + " has no matching remote location, aborting check-in");
    throw new CheckInException("Item's location doesn't match any remote location");
  }

  private String getPrimaryServicePoint(String locationId) {
    var location = locationClient.getLocation(locationId);
    if (StringUtils.isBlank(location.getPrimaryServicePoint())) {
      throw new CheckInException("Primary service point does not exist for locationId " + locationId);
    }
    return location.getPrimaryServicePoint();
  }
}
