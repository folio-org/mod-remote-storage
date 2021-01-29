package org.folio.rs.controller;

import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.folio.rs.TestBase;
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

public class LocationMappingsTest extends TestBase {

  private static final String MAPPINGS_URL = "http://localhost:%s/remote-storage/mappings/";

  private String mappingsUrl;

  @BeforeEach
  void prepareUrl() {
    mappingsUrl = String.format(MAPPINGS_URL, okapiPort);
  }

  @Test
  void canPostMapping() {
    ResponseEntity<LocationMapping> responseEntity = post(mappingsUrl,
        new LocationMapping()
          .folioLocationId(randomIdAsString())
          .configurationId(randomIdAsString()),
        LocationMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody().getFolioLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getConfigurationId(), notNullValue());
  }

  @Test
  void canGetAllMappings() {
    ResponseEntity<LocationMappings> responseEntity = get(mappingsUrl, LocationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(2));
  }

  @Test
  void canGetMappingById() {
    LocationMapping mapping = get(mappingsUrl, LocationMappings.class).getBody().getMappings().get(0);
    ResponseEntity<LocationMapping> response = get(mappingsUrl + mapping.getFolioLocationId(), LocationMapping.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canDeleteMapping() {
    ResponseEntity<LocationMapping> responseEntity = post(mappingsUrl,
        new LocationMapping()
          .folioLocationId(randomIdAsString())
          .configurationId(randomIdAsString()),
        LocationMapping.class);
    assertThat(delete(mappingsUrl + responseEntity.getBody().getFolioLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    HttpEntity invalidBody = new HttpEntity(new LocationMapping()
      .folioLocationId("abcde")
      .configurationId("abcde"));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> post(mappingsUrl, invalidBody, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnNotFoundForRandomUuid() {
    String urlWithRandomUuid = mappingsUrl + randomIdAsString();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    String urlWithRandomUuid = mappingsUrl + "abcde";
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
