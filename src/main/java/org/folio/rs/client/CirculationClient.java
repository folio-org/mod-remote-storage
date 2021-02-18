package org.folio.rs.client;

import org.folio.rs.domain.dto.CheckInCirculationRequest;

import org.folio.rs.domain.dto.ItemRequests;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient("circulation")
public interface CirculationClient {
  @PostMapping(value = "/check-in-by-barcode", consumes = MediaType.APPLICATION_JSON_VALUE)
  void checkIn(@RequestBody CheckInCirculationRequest checkInByBarcodeRequest);

  @GetMapping(value = "/requests/queue/{itemId}", consumes = MediaType.APPLICATION_JSON_VALUE)
  ItemRequests getItemRequests(@PathVariable("itemId") String itemId);
}
