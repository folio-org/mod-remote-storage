package org.folio.rs.client;

import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemsMove;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.error.ItemReturnException;
import org.folio.util.StringUtil;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;

@HttpExchange(value = "inventory")
public interface InventoryClient {

  @GetExchange("/instances")
  ResultList<Instance> getInstancesByQuery(@RequestParam("query") String query);

  @GetExchange("/instances/{id}")
  Instance getInstance(@PathVariable String id);

  @GetExchange("/items")
  ResultList<Item> getItemsByQuery(@RequestParam("query") String query);

  @PostExchange("/items/move")
  void moveItemsToHolding(@RequestBody ItemsMove itemsMove);

  @PutExchange("/items/{id}")
  void putItem(@PathVariable String id, @RequestBody Item item);

  @PostExchange("/items/{id}/mark-missing")
  void markItemAsMissing(@PathVariable String id);

  default Item getItemByBarcodeOrNull(String itemBarcode) {
    var items = getItemsByQuery("barcode==" + StringUtil.cqlEncode(itemBarcode));
    if (items.isEmpty()) {
      return null;
    }
    return items.getResult().getFirst();
  }

  default Item getItemByBarcode(String itemBarcode) {
    var item = getItemByBarcodeOrNull(itemBarcode);
    if (item == null) {
      throw new ItemReturnException("Item does not exist for barcode " + itemBarcode);
    }
    return item;
  }
}
