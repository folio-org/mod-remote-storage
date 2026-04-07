package org.folio.rs.client;

import org.folio.rs.domain.dto.ContributorType;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange(value = "contributor-types")
public interface ContributorTypesClient {
  @GetExchange
  ResultList<ContributorType> getContributorTypesByQuery(@RequestParam("query") String query);
}
