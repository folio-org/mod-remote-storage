package org.folio.rs.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;

import java.util.UUID;

public class LocationMappingsControllerTest extends ControllerTestBase {

  private static final String MAPPINGS_URL = "http://localhost:%s/remote-storage/mappings/";

  private String mappingsUrl;

  @BeforeEach
  void prepareUrl() {
    mappingsUrl = String.format(MAPPINGS_URL, port);
  }

  @Test
  void canPostMapping() {
    ResponseEntity<LocationMapping> responseEntity = restTemplate
      .postForEntity(mappingsUrl,
        new LocationMapping()
          .folioLocationId(UUID.randomUUID().toString())
          .configurationId(UUID.randomUUID().toString()),
        LocationMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getFolioLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getConfigurationId(), notNullValue());
  }

  @Test
  void canGetAllMappings() {
    ResponseEntity<LocationMappings> responseEntity = restTemplate
      .getForEntity(mappingsUrl, LocationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(1));
  }

  @Test
  void canGetMappingById() {
    LocationMapping mapping = restTemplate.getForObject(mappingsUrl, LocationMappings.class)
      .getMappings().get(0);
    ResponseEntity<LocationMapping> response = restTemplate
      .getForEntity(mappingsUrl + mapping.getFolioLocationId(), LocationMapping.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canDeleteMapping() {
    ResponseEntity<LocationMapping> responseEntity = restTemplate
      .postForEntity(mappingsUrl,
        new LocationMapping()
          .folioLocationId(UUID.randomUUID().toString())
          .configurationId(UUID.randomUUID().toString()),
        LocationMapping.class);
    assertThat(restTemplate.exchange(mappingsUrl + responseEntity.getBody().getFolioLocationId(), HttpMethod.DELETE,
      new HttpEntity<>(headers), String.class).getStatusCode(), is(HttpStatus.NO_CONTENT));
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    HttpEntity invalidBody = new HttpEntity(new LocationMapping()
      .folioLocationId("abcde")
      .configurationId("abcde"));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .postForEntity(mappingsUrl, invalidBody, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnNotFoundForRandomUuid() {
    String urlWithRandomUuid = mappingsUrl + UUID.randomUUID().toString();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    String urlWithRandomUuid = mappingsUrl + "abcde";
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
