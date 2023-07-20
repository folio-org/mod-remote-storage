package org.folio.rs.client;

import org.folio.rs.domain.entity.SystemUserParameters;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;

@FeignClient("authn")
public interface AuthnClient {

  @PostMapping(value = "/login", consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> getApiKey(@RequestBody SystemUserParameters systemUserParameters, @RequestHeader(TENANT) String tenantId);

  @PostMapping(value = "/credentials", consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveCredentials(@RequestBody SystemUserParameters systemUserParameters);

  @DeleteMapping(value = "/credentials", consumes = MediaType.APPLICATION_JSON_VALUE)
  void deleteCredentials(@RequestParam("userId") String userId);

}
