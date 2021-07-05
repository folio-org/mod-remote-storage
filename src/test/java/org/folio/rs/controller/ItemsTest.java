package org.folio.rs.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.folio.rs.TestBase;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;

public class ItemsTest extends TestBase {

  private static final String MARK_AS_MISSING_URL = "http://localhost:%s/remote-storage/items/barcode/%s/markAsMissing";
  private static final String MISSING_ITEM_BARCODE = "missing-item-barcode";

  @Test
  void canMarkItemAsMissing() {
    var markAsMissingUrl = String.format(MARK_AS_MISSING_URL, okapiPort, MISSING_ITEM_BARCODE);
    var response = post(markAsMissingUrl, HttpEntity.EMPTY, String.class);

    assertThat(response.getStatusCode(), is(HttpStatus.CREATED));
  }
}
