package org.folio.rs.client;

import org.folio.rs.domain.dto.ResultList;
import org.springframework.web.bind.annotation.RequestParam;

public interface QueryableClient<E> {
  ResultList<E> query(@RequestParam("query") String query);
}
