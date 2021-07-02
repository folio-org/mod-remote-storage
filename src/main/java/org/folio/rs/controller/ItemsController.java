package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.rest.resource.ItemsApi;
import org.folio.rs.service.ItemsService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class ItemsController implements ItemsApi {

  private final ItemsService itemsService;

  @Override
  public ResponseEntity<String> markItemAsMissingByBarcode(@PathVariable String barcode) {
    itemsService.markItemAsMissingByBarcode(barcode);
    return new ResponseEntity<>(String.format("Item with barcode=%s marked as missing", barcode), HttpStatus.CREATED);
  }
}
