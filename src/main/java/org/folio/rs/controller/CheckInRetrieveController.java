package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.rest.resource.RetrieveApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class CheckInRetrieveController implements RetrieveApi {

  private final CheckInItemService checkInItemService;

  @Override
  public ResponseEntity<String> checkInItemByBarcodeWithRemoteStorageConfigurationId(String remoteStorageConfigurationId, @Valid CheckInItem checkInItem) {
    checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    return new ResponseEntity<>("Check-in was done for " + checkInItem.getItemBarcode(), HttpStatus.OK);
  }
}
