package org.folio.rs.controller;

import javax.validation.Valid;

import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
import org.folio.rs.rest.resource.RetrieveApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

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

  @Override
  public ResponseEntity<String> checkInItemByHoldIdWithRemoteStorageConfigurationId(String remoteStorageConfigurationId, @Valid CheckInItemByHoldId checkInItemByHoldId) {
    checkInItemService.checkInItemByHoldId(remoteStorageConfigurationId, checkInItemByHoldId);
    return new ResponseEntity<>("Check-in was done for item with holdId" + checkInItemByHoldId.getHoldId(), HttpStatus.OK);
  }
}
