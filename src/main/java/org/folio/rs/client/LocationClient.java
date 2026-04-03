package org.folio.rs.client;

import org.folio.rs.domain.dto.FolioLocation;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("locations")
public interface LocationClient {

  @GetExchange("/{finalLocationId}")
  FolioLocation getLocation(@PathVariable String finalLocationId);

  @GetExchange
  ResultList<FolioLocation> getLocations(
    @RequestParam(value = "offset") int offset,
    @RequestParam(value = "limit") int limit);
}
