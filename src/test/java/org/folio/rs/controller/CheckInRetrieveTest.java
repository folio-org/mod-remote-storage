package org.folio.rs.controller;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
import org.folio.rs.domain.entity.LocationMapping;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.repository.LocationMappingsRepository;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;

public class CheckInRetrieveTest extends TestBase {

  private static final String CHECK_IN_URL = "http://localhost:%s/remote-storage/retrieve/%s/checkInItem";
  private static final String CHECK_IN_BY_HOLD_ID_URL = "http://localhost:%s/remote-storage/retrieve/%s/checkInItemByHoldId";
  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FINAL_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String HOLD_ID = "5c3c7621-8ad0-43ca-a675-4f2f32d25b27";
  private static final String HOLD_ID_NOT_FOUND = "d83bc442-6705-41dd-b199-07c3c2083db6";
  private static final String ITEM_BARCODE = "2887532577331";
  private static final String REMOTE_STORAGE_ERROR_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15628";

  private String checkInUrl;
  private String checkInByHoldIdUrl;
  private String errorCheckInUrl;
  private String errorCheckInByHoldIdUrl;

  @Autowired
  private LocationMappingsRepository locationMappingsRepository;

  @Autowired
  private ReturnRetrievalQueueRepository returnRetrievalQueueRepository;

  @BeforeEach
  void prepare() {
    LocationMapping locationMapping = new LocationMapping();
    locationMapping.setFinalLocationId(UUID.fromString(FINAL_LOCATION_ID));
    locationMapping.setRemoteConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID));
    locationMapping.setOriginalLocationId(UUID.fromString(FINAL_LOCATION_ID));
    locationMappingsRepository.save(locationMapping);
    returnRetrievalQueueRepository.save(ReturnRetrievalQueueRecord.builder()
      .id(UUID.randomUUID())
      .holdId(HOLD_ID)
      .remoteStorageId(stringToUUIDSafe(REMOTE_STORAGE_CONFIGURATION_ID))
      .itemBarcode(ITEM_BARCODE)
      .createdDateTime(LocalDateTime.now())
      .build());
    checkInUrl = String.format(CHECK_IN_URL, okapiPort, REMOTE_STORAGE_CONFIGURATION_ID);
    checkInByHoldIdUrl = String.format(CHECK_IN_BY_HOLD_ID_URL, okapiPort, REMOTE_STORAGE_CONFIGURATION_ID);
    errorCheckInUrl = String.format(CHECK_IN_URL, okapiPort, REMOTE_STORAGE_ERROR_CONFIGURATION_ID);
    errorCheckInByHoldIdUrl = String.format(CHECK_IN_BY_HOLD_ID_URL, okapiPort, REMOTE_STORAGE_ERROR_CONFIGURATION_ID);
  }

  @AfterEach
  void clear() {
    locationMappingsRepository.deleteById(UUID.fromString(FINAL_LOCATION_ID));
  }

  @Test
  void canCheckInItemByBarcodeWithRemoteStorageConfigurationIdPost() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);
    var response = post(checkInUrl, checkInItem, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void shouldReturnBadRequestForInvalidRemoteStorageId() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);
    var exception = assertThrows(HttpClientErrorException.class, () -> post(errorCheckInUrl, checkInItem, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void canCheckInItemByHoldIdWithRemoteStorageConfigurationIdPost() {
    var checkInByHoldId = getCheckInItemByHoldIdSample(HOLD_ID);
    var response = post(checkInByHoldIdUrl, checkInByHoldId, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void testCheckInByHoldIdForErrorRemoteStorageId() {
    var checkInByHoldId = getCheckInItemByHoldIdSample(HOLD_ID);
    var exception = assertThrows(HttpClientErrorException.class, () -> post(errorCheckInByHoldIdUrl, checkInByHoldId, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void testCheckInByHoldIdForErrorHoldId() {
    var checkInByHoldId = getCheckInItemByHoldIdSample(HOLD_ID_NOT_FOUND);
    var exception = assertThrows(HttpClientErrorException.class, () -> post(checkInByHoldIdUrl, checkInByHoldId, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void testCheckInByHoldIdForInvalidHoldId() {
    CheckInItemByHoldId checkInByHoldId = getCheckInItemByHoldIdSample("invalid-id");
    var exception = assertThrows(HttpClientErrorException.class, () -> post(checkInByHoldIdUrl, checkInByHoldId, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  private CheckInItemByHoldId getCheckInItemByHoldIdSample(String holdId) {
    var checkInByHoldId = new CheckInItemByHoldId();
    checkInByHoldId.setHoldId(holdId);
    return checkInByHoldId;
  }
}
