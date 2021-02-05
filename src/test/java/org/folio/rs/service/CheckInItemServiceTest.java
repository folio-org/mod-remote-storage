package org.folio.rs.service;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.domain.dto.CheckInByBarcodeRequest;
import org.folio.rs.domain.dto.ItemBarcode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class CheckInItemServiceTest {

  @Mock
  private CirculationClient circulationClient;

  @InjectMocks
  private CheckInItemService checkInItemService;

  @Test
  public void testCheckInItemByBarcodeIfCirculationClientSuccess() {
    when(circulationClient.checkIn(isA(CheckInByBarcodeRequest.class)))
      .thenReturn(new ResponseEntity<>(HttpStatus.OK));

    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    var actualStatus = checkInItemService.checkInItemByBarcode(itemBarcode);
    assertThat(actualStatus, equalTo(HttpStatus.OK));
  }

  @Test
  public void testCheckInItemByBarcodeIfCirculationClientFail() {
    when(circulationClient.checkIn(isA(CheckInByBarcodeRequest.class)))
      .thenReturn(new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR));

    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    var actualStatus = checkInItemService.checkInItemByBarcode(itemBarcode);
    assertThat(actualStatus, equalTo(HttpStatus.INTERNAL_SERVER_ERROR));
  }
}
