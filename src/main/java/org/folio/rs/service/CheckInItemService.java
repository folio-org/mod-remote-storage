package org.folio.rs.service;

import static java.lang.String.format;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
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

  public void checkInItemByBarcode(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.info("Start check-in process for item with barcode " + checkInItem.getItemBarcode());
    var locationMapping = mappingsRepository.findByRemoteStorageConfigurationId(UUID.fromString(remoteStorageConfigurationId));
    if (locationMapping.isEmpty()) {
      throw new CheckInException("Folio location does not exist for remoteStorageConfigurationId " + remoteStorageConfigurationId);
    } else {
      var finalLocationId = locationMapping.get().getFinalLocationId().toString();
      var folioLocation = locationClient.getLocation(finalLocationId);
      if (StringUtils.isBlank(folioLocation.getPrimaryServicePoint())) {
        throw new CheckInException("Primary service point is empty for remoteStorageConfigurationId " + remoteStorageConfigurationId);
      } else {
        circulationClient.checkIn(CheckInCirculationRequest.of(checkInItem.getItemBarcode(),
          folioLocation.getPrimaryServicePoint(), Instant.now().truncatedTo(ChronoUnit.MILLIS)));
        log.info("Check-in success for item with barcode " + checkInItem.getItemBarcode());
      }
    }
  }

  public void checkInItemByHoldId(String remoteStorageConfigurationId, CheckInItemByHoldId checkInItemByHoldId) {
    var holdId = checkInItemByHoldId.getHoldId();
    log.info("Start check-in process for item with associated request with id=" + holdId);
    var retrievalQueueRecord = returnRetrievalQueueService.getLastRetrievalByHoldId(holdId, remoteStorageConfigurationId);
    var barcode = retrievalQueueRecord
      .orElseThrow(() -> new CheckInException(
        format("Retrieval Queue Record with holdId=%s not found for remoteStorageId=%s", holdId, remoteStorageConfigurationId)))
      .getItemBarcode();
    var checkInItemByBarcode = new CheckInItem();
    checkInItemByBarcode.setItemBarcode(barcode);
    checkInItemByBarcode(remoteStorageConfigurationId, checkInItemByBarcode);
  }
}
