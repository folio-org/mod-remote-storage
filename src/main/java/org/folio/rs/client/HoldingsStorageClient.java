package org.folio.rs.client;

import org.folio.rs.domain.dto.HoldingsRecord;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;

@HttpExchange("holdings-storage")
public interface HoldingsStorageClient {

  @GetExchange("/holdings")
  ResultList<HoldingsRecord> getHoldingsRecordsByQuery(@RequestParam("query") String query);

  @PostExchange("/holdings")
  HoldingsRecord postHoldingsRecord(@RequestBody HoldingsRecord holdingsRecord);

  @GetExchange("/holdings/{id}")
  HoldingsRecord getHoldingsRecord(@PathVariable("id") String id);

  @PutExchange(value = "/holdings/{id}")
  void putHoldingsRecord(@PathVariable String id, @RequestBody HoldingsRecord holdingsRecord);
}
