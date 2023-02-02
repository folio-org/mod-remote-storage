package org.folio.rs.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;

import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.CheckInItemByHoldId;
import org.folio.rs.rest.resource.RetrieveApi;
import org.folio.rs.service.CheckInItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class CheckInRetrieveController implements RetrieveApi {

  private final CheckInItemService checkInItemService;

  @Override
  public ResponseEntity<String> checkInItemByBarcodeWithRemoteStorageConfigurationId(
      @Pattern(regexp = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$") @ApiParam(required = true) @PathVariable("remoteStorageConfigurationId") String remoteStorageConfigurationId,
      @ApiParam(required = true) @Valid @RequestBody CheckInItem checkInItem) {
    checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    return new ResponseEntity<>("Check-in was done for " + checkInItem.getItemBarcode(), HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> checkInItemByHoldIdWithRemoteStorageConfigurationId(
      @ApiParam(value = "", required = true) @PathVariable("remoteStorageConfigurationId") String remoteStorageConfigurationId,
      @ApiParam(required = true) @Valid @RequestBody CheckInItemByHoldId checkInItemByHoldId) {
    checkInItemService.checkInItemByHoldId(remoteStorageConfigurationId, checkInItemByHoldId);
    return new ResponseEntity<>("Check-in was done for item with holdId" + checkInItemByHoldId.getHoldId(), HttpStatus.OK);
  }
}
