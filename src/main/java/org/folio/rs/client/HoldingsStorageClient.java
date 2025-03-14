package org.folio.rs.client;

import org.folio.rs.domain.dto.HoldingsRecord;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@FeignClient(value = "holdings-storage")
public interface HoldingsStorageClient {
  @GetMapping("/holdings")
  ResultList<HoldingsRecord> getHoldingsRecordsByQuery(@RequestParam("query") String query);

  @PostMapping(value = "/holdings",
    consumes = MediaType.APPLICATION_JSON_VALUE)
  HoldingsRecord postHoldingsRecord(@RequestBody HoldingsRecord holdingsRecord);

  @GetMapping(value = "/holdings/{id}")
  HoldingsRecord getHoldingsRecord(@PathVariable("id") String id);

  @PutMapping(value = "/holdings/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
  void putHoldingsRecord(@PathVariable("id") String id, @RequestBody HoldingsRecord holdingsRecord);
}
