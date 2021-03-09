package org.folio.rs.controller;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;
import static org.folio.rs.service.LocationMappingsService.MAPPINGS;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;

public class LocationMappingsTest extends TestBase {

  private static final String MAPPINGS_URL = "http://localhost:%s/remote-storage/mappings/";

  private String mappingsUrl;

  @Autowired
  private CacheManager cacheManager;

  @BeforeEach
  void prepareUrl() {
    mappingsUrl = String.format(MAPPINGS_URL, okapiPort);
    ofNullable(cacheManager.getCache(MAPPINGS)).ifPresent(Cache::clear);
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

    // Verify caching
    LocationMapping mapping = get(mappingsUrl + "/" + responseEntity.getBody().getFolioLocationId(), LocationMapping.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(responseEntity.getBody(), mapping, true, StorageConfiguration.class, METADATA));
    assertTrue(EqualsBuilder.reflectionEquals(
      requireNonNull(
        requireNonNull(cacheManager.getCache(MAPPINGS)).get(responseEntity.getBody().getFolioLocationId())).get(), mapping, true, StorageConfiguration.class, METADATA));

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
    assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFolioLocationId()), nullValue());
    ResponseEntity<LocationMapping> firstResponse = get(mappingsUrl + mapping.getFolioLocationId(), LocationMapping.class);
    assertThat(firstResponse.getStatusCode(), is(HttpStatus.OK));

    // Verify cache
    Object cachedMapping = requireNonNull(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFolioLocationId())).get();

    ResponseEntity<LocationMapping> secondResponse = get(mappingsUrl + mapping.getFolioLocationId(), LocationMapping.class);
    assertThat(secondResponse.getStatusCode(), is(HttpStatus.OK));
    assertTrue(EqualsBuilder.reflectionEquals(mapping, secondResponse.getBody(), true, LocationMapping.class, "metadata"));
    assertTrue(EqualsBuilder.reflectionEquals(cachedMapping, secondResponse.getBody(), true, LocationMapping.class, "metadata"));
  }

  @Test
  void canDeleteMapping() {
    ResponseEntity<LocationMapping> responseEntity = post(mappingsUrl,
        new LocationMapping()
          .folioLocationId(randomIdAsString())
          .configurationId(randomIdAsString()),
        LocationMapping.class);
    assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFolioLocationId()), notNullValue());
    assertThat(delete(mappingsUrl + responseEntity.getBody().getFolioLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
    assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFolioLocationId()), nullValue());
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
