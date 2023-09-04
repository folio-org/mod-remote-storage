package org.folio.rs.client;

import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "users", contextId = "usersClientRS")
public interface UsersClient {

  @GetMapping
  ResultList<User> getUsersByQuery(@RequestParam("query") String query);

}
