package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.InventoryClient;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class ItemsService {
  private final InventoryClient inventoryClient;

  public void markItemAsMissingByBarcode(String barcode) {
    log.debug("markItemAsMissingByBarcode :: barcode:{}",barcode);
    var item = inventoryClient.getItemByBarcodeOrNull(barcode);
    if (item != null) {
      inventoryClient.markItemAsMissing(item.getId());
    }
  }
}
