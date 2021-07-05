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
    var items = inventoryClient.getItemsByQuery("barcode==" + barcode);
    if (!items.isEmpty()) {
      inventoryClient.markItemAsMissing(items.getResult().get(0).getId());
    }
  }
}
