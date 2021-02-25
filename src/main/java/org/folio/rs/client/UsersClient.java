package org.folio.rs.client;

import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient("users")
public interface UsersClient {

  @GetMapping
  ResultList<User> getUsersByQuery(@RequestParam("query") String query);

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveUser(@RequestBody User user);

  @PutMapping(value = "/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
  void updateUser(@PathVariable String id, @RequestBody User user);
}
