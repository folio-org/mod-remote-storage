package org.folio.rs.service;

import org.folio.rs.TestBase;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.domain.dto.CheckInByBarcodeRequest;
import org.folio.rs.domain.dto.ItemBarcode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class CheckInItemServiceTest extends TestBase {

  @Autowired
  private CheckInItemService checkInItemService;

  @Test
  public void testCheckInItemByBarcode() {
    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    var actualStatus = checkInItemService.checkInItemByBarcode(itemBarcode);
    assertThat(actualStatus, equalTo(HttpStatus.OK));
  }

  @Test
  public void testCheckInItemByBarcodeIfCirculationClientSuccess() {
   var circulationClient = Mockito.mock(CirculationClient.class);
    when(circulationClient.checkIn(isA(CheckInByBarcodeRequest.class)))
      .thenReturn(new ResponseEntity<>(HttpStatus.OK));
    CheckInItemService checkInItemService = new CheckInItemService(circulationClient);

    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    var actualStatus = checkInItemService.checkInItemByBarcode(itemBarcode);
    assertThat(actualStatus, equalTo(HttpStatus.OK));
  }

  @Test
  public void testCheckInItemByBarcodeIfCirculationClientFail() {
    var circulationClient = Mockito.mock(CirculationClient.class);
    when(circulationClient.checkIn(isA(CheckInByBarcodeRequest.class)))
      .thenReturn(new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR));
    CheckInItemService checkInItemService = new CheckInItemService(circulationClient);

    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    var actualStatus = checkInItemService.checkInItemByBarcode(itemBarcode);
    assertThat(actualStatus, equalTo(HttpStatus.INTERNAL_SERVER_ERROR));
  }
}
