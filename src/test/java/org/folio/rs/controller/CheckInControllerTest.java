package org.folio.rs.controller;

import org.folio.rs.TestBase;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class CheckInControllerTest extends TestBase {

  private static final String CHECK_IN_URL = "http://localhost:%s/remote-storage/check-in-by-barcode";

  private String checkInUrl;

  @BeforeEach
  void prepareUrl() {
    checkInUrl = String.format(CHECK_IN_URL, okapiPort);
  }

  @Test
  public void canPostCheckInByBarcodePost() {
    var itemBarcode = "{\"value\": \"2887532577331\"}";
    var response = post(checkInUrl, itemBarcode, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }
}
