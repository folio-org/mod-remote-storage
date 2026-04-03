package org.folio.rs.client;

import org.folio.rs.domain.dto.CheckInCirculationRequest;

import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;

@HttpExchange("circulation")
public interface CirculationClient {
  @PostExchange("/check-in-by-barcode")
  void checkIn(@RequestBody CheckInCirculationRequest checkInByBarcodeRequest);

  @GetExchange("/requests/queue/item/{itemId}")
  ResultList<Request> getItemRequests(@PathVariable String itemId);
}
