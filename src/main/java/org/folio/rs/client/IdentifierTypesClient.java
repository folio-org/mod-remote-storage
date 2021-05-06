package org.folio.rs.client;

import org.folio.rs.domain.dto.IdentifierType;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "identifier-types")
public interface IdentifierTypesClient {
  @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  ResultList<IdentifierType> getIdentifierTypesByQuery(@RequestParam("query") String query);
}
