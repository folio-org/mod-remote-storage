package org.folio.rs.client;

import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemsMove;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.error.ItemReturnException;
import org.folio.util.StringUtil;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "inventory")
public interface InventoryClient {
  @GetMapping("/instances")
  ResultList<Instance> getInstancesByQuery(@RequestParam("query") String query);

  @GetMapping(value = "/instances/{id}")
  Instance getInstance(@PathVariable("id") String id);

  @GetMapping(value = "/items", consumes = MediaType.APPLICATION_JSON_VALUE)
  ResultList<Item> getItemsByQuery(@RequestParam("query") String query);

  @PostMapping(value = "/items/move", consumes = MediaType.APPLICATION_JSON_VALUE)
  void moveItemsToHolding(@RequestBody ItemsMove itemsMove);

  @PutMapping(value = "/items/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
  void putItem(@PathVariable("id") String id, @RequestBody Item item);

  @PostMapping("/items/{id}/mark-missing")
  void markItemAsMissing(@PathVariable("id") String id);

  default Item getItemByBarcodeOrNull(String itemBarcode) {
    var items = getItemsByQuery("barcode==" + StringUtil.cqlEncode(itemBarcode));
    if (items.isEmpty()) {
      return null;
    }
    return items.getResult().get(0);
  }

  default Item getItemByBarcode(String itemBarcode) {
    var item = getItemByBarcodeOrNull(itemBarcode);
    if (item == null) {
      throw new ItemReturnException("Item does not exist for barcode " + itemBarcode);
    }
    return item;
  }
}
