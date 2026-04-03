package org.folio.rs.client;

import org.folio.rs.domain.dto.IdentifierType;
import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange(value = "identifier-types")
public interface IdentifierTypesClient {

  @GetExchange
  ResultList<IdentifierType> getIdentifierTypesByQuery(@RequestParam("query") String query);
}
