package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.rest.resource.RemoteStorageConfigurationIdApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class CheckInController implements RemoteStorageConfigurationIdApi {

  private final CheckInItemService checkInItemService;

  @Override
  public ResponseEntity<String> checkInItemByBarcodeWithRemoteStorageConfigurationId(String remoteStorageConfigurationId, @Valid CheckInItem checkInItem) {
    var status = checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    if (status == HttpStatus.OK) {
      return new ResponseEntity<>("Check-in was done for " + checkInItem.getItemBarcode(), HttpStatus.OK);
    }
    return new ResponseEntity<>("Server error while check-in for " + checkInItem.getItemBarcode(), HttpStatus.INTERNAL_SERVER_ERROR);
  }
}
