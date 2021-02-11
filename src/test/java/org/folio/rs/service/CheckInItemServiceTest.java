package org.folio.rs.service;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.FolioLocation;
import org.folio.rs.domain.entity.LocationMapping;
import org.folio.rs.repository.LocationMappingsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class CheckInItemServiceTest {

  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FOLIO_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String PRIMARY_SERVICE_POINT = "79faacf1-4ba4-42c7-8b2a-566b259e4641";
  @Mock
  private CirculationClient circulationClient;
  @Mock
  private LocationMappingsRepository locationMappingsRepository;
  @Mock
  private LocationClient locationClient;

  @InjectMocks
  private CheckInItemService checkInItemService;

  private LocationMapping locationMapping;
  private FolioLocation folioLocation;
  private CheckInItem checkInItem;

  @BeforeEach
  public void prepare() {
    locationMapping = new LocationMapping();
    locationMapping.setFolioLocationId(UUID.fromString(FOLIO_LOCATION_ID));
    locationMapping.setConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID));
    folioLocation = FolioLocation.of(FOLIO_LOCATION_ID, PRIMARY_SERVICE_POINT);
    checkInItem = new CheckInItem();
    checkInItem.setItemBarcode("item-barcode");
  }

  @Test
  void testCheckInItemByBarcode() {
    when(locationMappingsRepository.getFirstByConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Optional.of(locationMapping));
    when(locationClient.getLocation(FOLIO_LOCATION_ID))
      .thenReturn(folioLocation);

    checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(circulationClient, times(1)).checkIn(isA(CheckInCirculationRequest.class));
  }

  @Test
  void testCheckInItemByBarcodeIfLocationClientReturnEmptyPrimaryServicePoint() {
    var folioLocation = FolioLocation.of(FOLIO_LOCATION_ID, "");

    when(locationMappingsRepository.getFirstByConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Optional.of(locationMapping));
    when(locationClient.getLocation(FOLIO_LOCATION_ID))
      .thenReturn(folioLocation);

    checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(circulationClient, times(0)).checkIn(isA(CheckInCirculationRequest.class));
  }

  @Test
  void testCheckInItemByBarcodeIfLocationNotExistInDataBase() {
    when(locationMappingsRepository.getFirstByConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Optional.empty());

    checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(circulationClient, times(0)).checkIn(isA(CheckInCirculationRequest.class));
  }
}
