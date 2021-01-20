package org.folio.rs.client;

import org.folio.rs.domain.entity.Credential;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;

@FeignClient("authn")
public interface AuthnClient {

  @PostMapping(value = "/login", produces = MediaType.APPLICATION_JSON_VALUE,
    consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> getApiKey(Credential credential);

  @PostMapping(value = "/credentials", consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveCredentials(Credential credential);

}
