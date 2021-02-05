package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.domain.dto.ItemBarcode;
import org.folio.rs.rest.resource.CheckInByBarcodeApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class CheckInController implements CheckInByBarcodeApi {

  private final CheckInItemService checkInItemService;

  @Override
  public ResponseEntity<String> checkInByBarcodePost(ItemBarcode itemBarcode) {
    var status = checkInItemService.checkInItemByBarcode(itemBarcode);
    if (status == HttpStatus.OK) {
      return new ResponseEntity <>("Check-in was done for " + itemBarcode.getValue(), HttpStatus.OK);
    }
    return new ResponseEntity<>("Server error while check-in for " + itemBarcode.getValue(), HttpStatus.INTERNAL_SERVER_ERROR);
  }
}
