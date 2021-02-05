package org.folio.rs.controller;

import org.folio.rs.domain.dto.ItemBarcode;
import org.folio.rs.service.CheckInItemService;
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
public class CheckInControllerTest {

  @Mock
  private CheckInItemService checkInItemService;

  @InjectMocks
  private CheckInController checkInController;

  @Test
  public void testCheckInByBarcodePost() {
    when(checkInItemService.checkInItemByBarcode(isA(ItemBarcode.class))).thenReturn(HttpStatus.OK);

    var itemBarcode = new ItemBarcode();
    itemBarcode.setValue("item-barcode");
    checkInController.checkInByBarcodePost(itemBarcode);

    var actualResponse = checkInController.checkInByBarcodePost(itemBarcode);
    assertThat(actualResponse, equalTo(new ResponseEntity<>(HttpStatus.OK)));
  }
}
