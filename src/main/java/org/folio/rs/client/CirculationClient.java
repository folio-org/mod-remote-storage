package org.folio.rs.client;

import org.folio.rs.domain.dto.CheckInCirculationRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient("circulation")
public interface CirculationClient {
  @PostMapping(value = "/check-in-by-barcode", consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> checkIn(@RequestBody CheckInCirculationRequest checkInByBarcodeRequest);
}
