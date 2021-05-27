package org.folio.rs.controller;

import static java.util.Optional.ofNullable;
import static org.folio.rs.service.LocationMappingsService.MAPPINGS;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.FullMapping;
import org.folio.rs.domain.dto.FullMappings;
import org.folio.rs.domain.dto.PlainMapping;
import org.folio.rs.domain.dto.PlainMappings;
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

import java.util.Arrays;

public class LocationMappingsTest extends TestBase {

  private static final String MAPPINGS_URL = "http://localhost:%s/remote-storage/mappings/";
  private static final String FULL_MAPPINGS_URL = "http://localhost:%s/remote-storage/full-mappings/";
  private static final String MAPPINGS_LOCATIONS_URL = "http://localhost:%s/remote-storage/full-mappings/locations";
  private static final String LOCATIONS_URL = "http://localhost:%s/locations";

  private String mappingsUrl;
  private String fullMappingsUrl;
  private String mappingsLocationsUrl;
  private String locationsUrl;

  @Autowired
  private CacheManager cacheManager;

  @BeforeEach
  void prepareUrl() {
    mappingsUrl = String.format(MAPPINGS_URL, okapiPort);
    fullMappingsUrl = String.format(FULL_MAPPINGS_URL, okapiPort);
    mappingsLocationsUrl = String.format(MAPPINGS_LOCATIONS_URL, okapiPort);
    locationsUrl = String.format(LOCATIONS_URL, okapiPort);
    ofNullable(cacheManager.getCache(MAPPINGS)).ifPresent(Cache::clear);
  }

  @Test
  void canPostPlainMapping() {
    var responseEntity = post(mappingsUrl, new PlainMapping().folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()), PlainMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody().getFolioLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getConfigurationId(), notNullValue());

    var mapping = get(mappingsUrl + "/" + responseEntity.getBody()
      .getFolioLocationId(), PlainMapping.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(responseEntity.getBody(), mapping, true, StorageConfiguration.class, METADATA));
  }

  @Test
  void canGetAllPlainMappings() {
    var mapping = post(mappingsUrl, new PlainMapping()
      .folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()),
      PlainMapping.class);
    var responseEntity = get(mappingsUrl, PlainMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertFalse(responseEntity.getBody().getMappings().isEmpty());

    delete(mappingsUrl + mapping.getBody().getFolioLocationId());
  }

  @Test
  void canGetPlainMappingById() {
    var mapping = get(mappingsUrl, PlainMappings.class).getBody()
      .getMappings()
      .get(0);

    // Verify cache disable via MODRS-42
    // Object cachedMapping =
    // requireNonNull(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFinalLocationId())).get();

    var response = get(mappingsUrl + mapping.getFolioLocationId(), PlainMapping.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getBody(), true, PlainMapping.class, "metadata"));
  }

  @Test
  void canDeletePlainMapping() {
    var responseEntity = post(mappingsUrl, new PlainMapping().folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()), PlainMapping.class);
    // Verify cache disable via MODRS-42
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // notNullValue());
    assertThat(delete(mappingsUrl + responseEntity.getBody()
      .getFolioLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
  }

  @Test
  void canPostFullMapping() {
    var responseEntity = post(fullMappingsUrl, new FullMapping().finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString()).originalLocationId(randomIdAsString()), FullMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody()
      .getFinalLocationId(), notNullValue());
    assertThat(responseEntity.getBody()
      .getRemoteConfigurationId(), notNullValue());


    var mapping = get(fullMappingsUrl + "/" + responseEntity.getBody().getFinalLocationId(),
      FullMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(responseEntity.getBody(), mapping.getMappings().get(0), true, FullMapping.class, METADATA));
    // Verify caching disable via MODRS-42
    // assertTrue(EqualsBuilder.reflectionEquals(
    // requireNonNull(
    // requireNonNull(cacheManager.getCache(MAPPINGS)).get(responseEntity.getBody().getFinalLocationId())).get(), mapping, true,
    // StorageConfiguration.class, METADATA));
  }

  @Test
  void canGetAllFullMappings() {
    var mapping = post(fullMappingsUrl, new FullMapping()
      .finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
      .originalLocationId(randomIdAsString()),
      FullMapping.class);
    ResponseEntity<FullMappings> responseEntity = get(fullMappingsUrl, FullMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertFalse(responseEntity.getBody().getMappings().isEmpty());

    delete(fullMappingsUrl + mapping.getBody().getFinalLocationId());
  }

  @Test
  void canGetMappingsLocations() {
    ResponseEntity<FullMappings> responseEntity = get(mappingsLocationsUrl, FullMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(2));
    assertThat(responseEntity.getBody().getMappings().get(0).getOriginalLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getMappings().get(1).getFinalLocationId(), nullValue());
  }

  @Test
  void canGetFullMappingsById() {
    var finalLocationId = randomIdAsString();
    var remoteConfigurationId = randomIdAsString();
    var mapping1 = post(fullMappingsUrl, new FullMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(randomIdAsString()),
      FullMapping.class).getBody();
    var mapping2 = post(fullMappingsUrl, new FullMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(randomIdAsString()),
      FullMapping.class).getBody();

    var response = get(fullMappingsUrl + finalLocationId, FullMappings.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertThat(response.getBody().getMappings().size(), is(2));
    assertTrue(Arrays.asList(mapping1, mapping2).containsAll(response.getBody().getMappings()));
    // Verify cache disable via MODRS-42
    // Object cachedMapping =
    // requireNonNull(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFinalLocationId())).get();
    // assertTrue(EqualsBuilder.reflectionEquals(cachedMapping, secondResponse.getBody(), true, LocationMapping.class, "metadata"));

    delete(fullMappingsUrl + finalLocationId);
  }

  @Test
  void canDeleteFullMapping() {
    var response = post(fullMappingsUrl, new FullMapping().finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
      .originalLocationId(randomIdAsString()), FullMapping.class);
    assertThat(delete(fullMappingsUrl + response.getBody()
      .getFinalLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
    // Verify cache disable via MODRS-42
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // notNullValue());
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // nullValue());
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    var invalidBody1 = new HttpEntity(new PlainMapping().folioLocationId("abcde")
      .configurationId("abcde"));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> post(mappingsUrl, invalidBody1, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));

    var invalidBody2 = new HttpEntity(new PlainMapping().folioLocationId("abcde")
      .configurationId("abcde"));
    exception = assertThrows(HttpClientErrorException.class,
      () -> post(mappingsUrl, invalidBody2, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnNotFoundForRandomUuid() {
    String urlWithRandomUuid1 = mappingsUrl + randomIdAsString();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid1));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    String urlWithRandomUuid2 = fullMappingsUrl + randomIdAsString();
    exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid2));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    String urlWithRandomUuid = mappingsUrl + "abcde";
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
