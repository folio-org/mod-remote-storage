package org.folio.rs.client;

import org.folio.rs.dto.ItemRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;


@FeignClient("circulation")
public interface ItemRequestsClient {

  @GetMapping(value = "/requests/queue/{itemId}", consumes = MediaType.APPLICATION_JSON_VALUE)
  ItemRequest getItemRequests(@PathVariable("itemId") String itemId);
}
