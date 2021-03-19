package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import org.folio.rs.domain.dto.Provider;
import org.folio.rs.rest.resource.ProvidersApi;
import org.folio.rs.service.ProvidersService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class ProviderController implements ProvidersApi{

  private final ProvidersService providersService;

  @GetMapping(value = "/providers")
  public ResponseEntity<List<Provider>> getProviders() {
    return new ResponseEntity<>(providersService.getProviders(), HttpStatus.OK);
  }
}
