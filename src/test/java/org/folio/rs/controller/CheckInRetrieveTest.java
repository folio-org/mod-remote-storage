package org.folio.rs.controller;

import org.folio.rs.TestBase;
import org.folio.rs.domain.entity.LocationMapping;
import org.folio.rs.repository.LocationMappingsRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;

import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class CheckInRetrieveTest extends TestBase {

  private static final String CHECK_IN_URL = "http://localhost:%s/remote-storage/retrieve/%s/checkInItem";
  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String FOLIO_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";

  private String checkInUrl;

  @Autowired
  private LocationMappingsRepository locationMappingsRepository;

  @BeforeEach
  void prepare() {
    LocationMapping locationMapping = new LocationMapping();
    locationMapping.setFolioLocationId(UUID.fromString(FOLIO_LOCATION_ID));
    locationMapping.setConfigurationId(UUID.fromString(REMOTE_STORAGE_CONFIGURATION_ID));
    locationMappingsRepository.save(locationMapping);
    checkInUrl = String.format(CHECK_IN_URL, okapiPort, REMOTE_STORAGE_CONFIGURATION_ID);
  }

  @AfterEach
  void clear() {
    locationMappingsRepository.deleteById(UUID.fromString(FOLIO_LOCATION_ID));
  }

  @Test
  void canCheckInItemByBarcodeWithRemoteStorageConfigurationIdPost() {
    var itemBarcode = "{\"itemBarcode\": \"2887532577331\"}";
    var response = post(checkInUrl, itemBarcode, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }
}
