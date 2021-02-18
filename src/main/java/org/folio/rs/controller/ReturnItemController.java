package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.ReturnItemResponse;
import org.folio.rs.rest.resource.ReturnApi;
import org.folio.rs.service.ReturnItemService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class ReturnItemController implements ReturnApi {

  private final ReturnItemService returnItemService;

  @Override
  public ResponseEntity<ReturnItemResponse> returnItemByBarcode(String remoteStorageConfigurationId, @Valid CheckInItem checkInItem) {
    return new ResponseEntity<>(returnItemService.returnItem(remoteStorageConfigurationId, checkInItem), HttpStatus.OK);
  }
}
