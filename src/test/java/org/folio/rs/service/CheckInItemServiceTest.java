package org.folio.rs.service;

import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
import org.folio.rs.domain.dto.FolioLocation;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.error.CheckInException;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import joptsimple.internal.Strings;

@ExtendWith(MockitoExtension.class)
public class CheckInItemServiceTest {

  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FOLIO_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String PRIMARY_SERVICE_POINT = "79faacf1-4ba4-42c7-8b2a-566b259e4641";
  private static final String HOLD_ID = "bdf5534b-685e-4f71-9fe6-cc2313504508";
  private static final String ITEM_BARCODE = "item-barcode";
  @Mock
  private CirculationClient circulationClient;
  @Mock
  private ExtendedRemoteLocationConfigurationMappingsRepository mappingsRepository;
  @Mock
  private LocationClient locationClient;
  @Mock
  private ReturnRetrievalQueueService returnRetrievalQueueService;

  @InjectMocks
  private CheckInItemService checkInItemService;

  private RemoteLocationConfigurationMappingEntity entity;
  private FolioLocation folioLocation;
  private CheckInItem checkInItem;
  private CheckInItemByHoldId checkInItemByHoldId;
  private ReturnRetrievalQueueRecord returnRetrievalQueueRecord;

  @BeforeEach
  public void prepare() {
    entity = RemoteLocationConfigurationMappingEntity.of(UUID.fromString(FOLIO_LOCATION_ID), UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID), Collections.emptySet());
    folioLocation = FolioLocation.of(FOLIO_LOCATION_ID, Strings.EMPTY, PRIMARY_SERVICE_POINT);
    returnRetrievalQueueRecord = ReturnRetrievalQueueRecord.builder().id(UUID.randomUUID()).holdId(HOLD_ID).itemBarcode(ITEM_BARCODE).build();
    checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);
    checkInItemByHoldId = new CheckInItemByHoldId();
    checkInItemByHoldId.setHoldId(HOLD_ID);
  }

  @Test
  void testCheckInItemByBarcode() {
    when(mappingsRepository.findAllByRemoteStorageConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Collections.singletonList(entity));
    when(locationClient.getLocation(FOLIO_LOCATION_ID))
      .thenReturn(folioLocation);

    checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(circulationClient, times(1)).checkIn(isA(CheckInCirculationRequest.class));
  }

  @Test
  void testCheckInItemByHoldId() {
    when(mappingsRepository.findAllByRemoteStorageConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Collections.singletonList(entity));
    when(locationClient.getLocation(FOLIO_LOCATION_ID))
      .thenReturn(folioLocation);
    when(returnRetrievalQueueService.getLastRetrievalByHoldId(HOLD_ID, REMOTE_STORAGE_CONFIGURATION_ID))
      .thenReturn(Optional.of(returnRetrievalQueueRecord));

    checkInItemService.checkInItemByHoldId(REMOTE_STORAGE_CONFIGURATION_ID, checkInItemByHoldId);

    verify(circulationClient, times(1)).checkIn(isA(CheckInCirculationRequest.class));
  }


  @Test
  void testCheckInItemByBarcodeIfLocationNotExistInDataBase() {
    when(mappingsRepository.findAllByRemoteStorageConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Collections.emptyList());

    Assertions.assertThrows(CheckInException.class,
      () -> checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }

  @Test
  void testCheckInItemByBarcodeIfLocationClientReturnEmptyPrimaryServicePoint() {
    var folioLocation = FolioLocation.of(FOLIO_LOCATION_ID, Strings.EMPTY, Strings.EMPTY);

    when(mappingsRepository.findAllByRemoteStorageConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID)))
      .thenReturn(Collections.singletonList(entity));
    when(locationClient.getLocation(FOLIO_LOCATION_ID))
      .thenReturn(folioLocation);

    Assertions.assertThrows(CheckInException.class,
      () -> checkInItemService.checkInItemByBarcode(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }
}
