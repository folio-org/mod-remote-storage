package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.error.CheckInException;
import org.folio.rs.repository.LocationMappingsRepository;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;


@Service
@RequiredArgsConstructor
@Log4j2
public class CheckInItemService {

  private final CirculationClient circulationClient;
  private final LocationMappingsRepository locationMappingsRepository;
  private final LocationClient locationClient;

  public void checkInItemByBarcode(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.info("Start check-in process for item with barcode " + checkInItem.getItemBarcode());
    var locationMapping = locationMappingsRepository
      .getFirstByConfigurationId(UUID.fromString(remoteStorageConfigurationId));
    if (locationMapping.isEmpty()) {
      throw new CheckInException("Folio location does not exist for remoteStorageConfigurationId " + remoteStorageConfigurationId);
    } else {
      var folioLocationId = locationMapping.get().getFolioLocationId().toString();
      var folioLocation = locationClient.getLocation(folioLocationId);
      if (StringUtils.isBlank(folioLocation.getPrimaryServicePoint())) {
        throw new CheckInException("Primary service point is empty for remoteStorageConfigurationId " + remoteStorageConfigurationId);
      } else {
        circulationClient.checkIn(CheckInCirculationRequest.of(checkInItem.getItemBarcode(),
          folioLocation.getPrimaryServicePoint(), Instant.now().truncatedTo(ChronoUnit.MILLIS)));
        log.info("Check-in success for item with barcode " + checkInItem.getItemBarcode());
      }
    }
  }
}
