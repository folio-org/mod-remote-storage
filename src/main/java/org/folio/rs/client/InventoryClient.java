package org.folio.rs.client;

import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "inventory")
public interface InventoryClient {
  @GetMapping("/instances")
  ResultList<Instance> getInstancesByQuery(@RequestParam("query") String query);

  @GetMapping(value = "/items", consumes = MediaType.APPLICATION_JSON_VALUE)
  ResultList<Item> getItemsByQuery(@RequestParam("query") String query);
}
