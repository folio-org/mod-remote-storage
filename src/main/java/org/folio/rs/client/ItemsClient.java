package org.folio.rs.client;

import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.Item;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "inventory")
public interface ItemsClient extends QueryableClient<Item> {
  @Override
  @GetMapping(value = "/items", consumes = MediaType.APPLICATION_JSON_VALUE)
  ResultList<Item> query(@RequestParam("query") String query);
}
