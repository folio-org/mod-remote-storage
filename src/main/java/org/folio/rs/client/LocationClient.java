package org.folio.rs.client;

import org.folio.rs.domain.dto.FolioLocation;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient("locations")
public interface LocationClient {

  @GetMapping(value = "/{finalLocationId}", consumes = MediaType.APPLICATION_JSON_VALUE)
  FolioLocation getLocation(@PathVariable("finalLocationId") String finalLocationId);

  @GetMapping
  ResultList<FolioLocation> getLocations();

}
