package org.folio.rs.controller;

import static org.folio.rs.TestUtils.ITEM_BARCODE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Objects;
import java.util.stream.Collectors;

import org.folio.rs.TestBase;
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
  public static final String ITEM2_BARCODE = "645398607548";
  public static final String ITEM3_BARCODE = "645398607549";
  private static final String RETURN_URL = "http://localhost:%s/remote-storage/return/%s";
  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FOLIO_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String FOLIO_LOCATION_2_ID = "dd7dd6cd-b04a-41d5-9b52-656f4aab8717";
  private static final String PRIMARY_SERVICE_POINT_2 = "e90d4ab8-ad13-4010-a28e-c36662e031ff";
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
    locationMappingsService.postRemoteLocationConfigurationMapping(new RemoteLocationConfigurationMapping()
      .folioLocationId(FOLIO_LOCATION_2_ID).configurationId(REMOTE_STORAGE_CONFIGURATION_ID));
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
  void shouldCheckInItemAtAppropriateServicePointWhenMultipleRemoteLocationMappingsExist() {
    locationMappingsService.postRemoteLocationConfigurationMapping(new RemoteLocationConfigurationMapping()
      .folioLocationId(FOLIO_LOCATION_2_ID).configurationId(REMOTE_STORAGE_CONFIGURATION_ID));

    var checkInItemJsonString = "{\"itemBarcode\": \"" + ITEM2_BARCODE + "\"}";
    var response = post(checkInUrl, checkInItemJsonString, ReturnItemResponse.class);

    assertThat(response.getStatusCode(), is(HttpStatus.OK));

    var bodyStrings = wireMockServer.getAllServeEvents().stream()
        .filter(e -> "/circulation/check-in-by-barcode".equals(e.getRequest().getUrl()))
        .map(e -> e.getRequest().getBody())
        .map(String::new)
        .collect(Collectors.toList());
    assertThat(bodyStrings.size(), is(1));
    assertTrue(bodyStrings.get(0).contains(PRIMARY_SERVICE_POINT_2));

    locationMappingsService.deleteMappingById(FOLIO_LOCATION_2_ID);
  }

  @Test
  void shouldReturnBadRequestIfItemEffectiveLocationDoesNotMatchRemoteLocation() {
    var itemBarcode = "{\"itemBarcode\":" + ITEM3_BARCODE + "}";
    var exception = assertThrows(HttpClientErrorException.class, () -> post(errorCheckInUrl, itemBarcode, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldReturnBadRequestForBarcodeWhenItemNotExist() {
    var itemBarcode = "{\"itemBarcode\": \"not-exist-item-barcode\"}";
    var exception = assertThrows(HttpClientErrorException.class, () -> post(errorCheckInUrl, itemBarcode, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
