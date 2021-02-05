package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.ItemBarcode;
import org.folio.rs.rest.resource.CheckInByBarcodeApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Log4j2
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class CheckInController implements CheckInByBarcodeApi {

  private final CheckInItemService checkInItemService;

  @Override
  public ResponseEntity<String> checkInByBarcodePost(ItemBarcode itemBarcode) {
    var status = checkInItemService.checkInItemByBarcode(itemBarcode);
    return new ResponseEntity<>(status);
  }
}
