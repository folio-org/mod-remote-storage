package org.folio.rs.controller;

import static org.folio.rs.TestUtils.ITEM_BARCODE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Objects;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ReturnItemResponse;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.rs.service.LocationMappingsService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpClientErrorException;

public class ReturnItemTest extends TestBase {

  private static final String RETURN_URL = "http://localhost:%s/remote-storage/return/%s";
  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FOLIO_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String REMOTE_STORAGE_ERROR_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15628";

  private String checkInUrl;
  private String errorCheckInUrl;

  @Autowired
  private LocationMappingsService locationMappingsService;
  @Autowired
  private ReturnRetrievalQueueRepository returnRetrievalQueueRepository;

  @BeforeEach
  void prepare() {
    locationMappingsService.postRemoteLocationConfigurationMapping(new RemoteLocationConfigurationMapping()
      .folioLocationId(FOLIO_LOCATION_ID).configurationId(REMOTE_STORAGE_CONFIGURATION_ID));
    checkInUrl = String.format(RETURN_URL, okapiPort, REMOTE_STORAGE_CONFIGURATION_ID);
    errorCheckInUrl = String.format(RETURN_URL, okapiPort, REMOTE_STORAGE_ERROR_CONFIGURATION_ID);
  }

  @AfterEach
  void clear() {
    locationMappingsService.deleteMappingById(FOLIO_LOCATION_ID);
    returnRetrievalQueueRepository.deleteAll();
  }

  @Test
  void canReturnItemByBarcodePost() {
    var itemBarcode = "{\"itemBarcode\": \"" + ITEM_BARCODE + "\"}";
    var response = post(checkInUrl, itemBarcode, ReturnItemResponse.class);

    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertTrue(Objects.requireNonNull(response.getBody()).getIsHoldRecallRequestExist());

    var records = returnRetrievalQueueRepository.findAll();
    boolean exist = records.stream().anyMatch(record -> ITEM_BARCODE.equals(record.getItemBarcode()));
    assertTrue(exist);
  }

  @Test
  void shouldReturnBadRequestForBarcodeWhenItemNotExist() {
    var itemBarcode = "{\"itemBarcode\": \"not-exist-item-barcode\"}";
    var exception = assertThrows(HttpClientErrorException.class, () -> post(errorCheckInUrl, itemBarcode, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
