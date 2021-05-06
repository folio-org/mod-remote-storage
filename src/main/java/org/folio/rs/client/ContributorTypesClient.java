package org.folio.rs.client;

import org.folio.rs.domain.dto.ContributorType;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "contributor-types")
public interface ContributorTypesClient {
  @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  ResultList<ContributorType> getContributorTypesByQuery(@RequestParam("query") String query);
}
