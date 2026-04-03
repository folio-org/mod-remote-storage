package org.folio.rs.client;

import java.util.Optional;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("users")
public interface RemoteStorageUsersClient {

  @GetExchange
  ResultList<User> getUsersByQuery(@RequestParam("query") String query);

  @GetExchange("/{id}")
  Optional<User> getUser(@PathVariable String id);
}
