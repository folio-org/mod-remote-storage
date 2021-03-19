package org.folio.rs.controller;

import org.folio.rs.TestBase;
import org.junit.jupiter.api.Test;
import org.springframework.http.ResponseEntity;

import java.util.List;

import static java.util.Objects.requireNonNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ProviderControllerTest extends TestBase {

  private static final String PROVIDERS_URL = "http://localhost:%s/remote-storage/providers/";

  @Test
  void shouldReturnAllProviders() {
    ResponseEntity<List> response = get(String.format(PROVIDERS_URL, okapiPort), List.class);
    assertEquals(3, requireNonNull(response.getBody()).size());
  }
}
