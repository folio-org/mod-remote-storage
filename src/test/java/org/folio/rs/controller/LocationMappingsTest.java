package org.folio.rs.controller;

import static java.util.Optional.ofNullable;
import static org.folio.rs.service.LocationMappingsService.MAPPINGS;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.in;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
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
import java.util.Set;

public class LocationMappingsTest extends TestBase {

  private static final String MAPPINGS_URL = "http://localhost:%s/remote-storage/mappings/";
  private static final String EXTENDED_MAPPINGS_URL = "http://localhost:%s/remote-storage/extended-mappings/";
  private static final String MAPPINGS_LOCATIONS_URL = "http://localhost:%s/remote-storage/extended-mappings/locations";
  private static final String LOCATIONS_URL = "http://localhost:%s/locations";

  private String mappingsUrl;
  private String extendedMappingsUrl;
  private String mappingsLocationsUrl;
  private String locationsUrl;

  @Autowired
  private CacheManager cacheManager;

  @BeforeEach
  void prepareUrl() {
    mappingsUrl = String.format(MAPPINGS_URL, okapiPort);
    extendedMappingsUrl = String.format(EXTENDED_MAPPINGS_URL, okapiPort);
    mappingsLocationsUrl = String.format(MAPPINGS_LOCATIONS_URL, okapiPort);
    locationsUrl = String.format(LOCATIONS_URL, okapiPort);
    ofNullable(cacheManager.getCache(MAPPINGS)).ifPresent(Cache::clear);
  }

  @Test
  void canPostRemoteLocationConfigurationMapping() {
    var responseEntity = post(mappingsUrl, new RemoteLocationConfigurationMapping().folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()), RemoteLocationConfigurationMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody().getFolioLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getConfigurationId(), notNullValue());

    var mapping = get(mappingsUrl + "/" + responseEntity.getBody()
      .getFolioLocationId(), RemoteLocationConfigurationMapping.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(responseEntity.getBody(), mapping, true, StorageConfiguration.class, METADATA));
  }

  @Test
  void canGetMappingsByCriteria() {
    var mapping = post(mappingsUrl, new RemoteLocationConfigurationMapping()
        .folioLocationId(randomIdAsString()).configurationId(randomIdAsString()), RemoteLocationConfigurationMapping.class)
      .getBody();

    var response = get(mappingsUrl + "?finalLocationId=" + mapping.getFolioLocationId(), RemoteLocationConfigurationMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getMappings().get(0), true, RemoteLocationConfigurationMapping.class));

    response = get(mappingsUrl + "?remoteStorageConfigurationId=" + mapping.getConfigurationId(), RemoteLocationConfigurationMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getMappings().get(0), true, RemoteLocationConfigurationMapping.class));

    delete(mappingsUrl + "/" + mapping.getFolioLocationId());
  }

  @Test
  void canGetAllRemoteLocationConfigurationMappings() {
    var mapping = post(mappingsUrl, new RemoteLocationConfigurationMapping()
      .folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()),
      RemoteLocationConfigurationMapping.class);
    var responseEntity = get(mappingsUrl, RemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertFalse(responseEntity.getBody().getMappings().isEmpty());

    delete(mappingsUrl + mapping.getBody().getFolioLocationId());
  }

  @Test
  void canGetRemoteLocationConfigurationMappingById() {
    var mapping = get(mappingsUrl, RemoteLocationConfigurationMappings.class).getBody()
      .getMappings()
      .get(0);

    // Verify cache disable via MODRS-42
    // Object cachedMapping =
    // requireNonNull(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFinalLocationId())).get();

    var response = get(mappingsUrl + mapping.getFolioLocationId(), RemoteLocationConfigurationMapping.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getBody(), true, RemoteLocationConfigurationMapping.class, "metadata"));
  }

  @Test
  void canDeleteRemoteLocationConfigurationMapping() {
    var responseEntity = post(mappingsUrl, new RemoteLocationConfigurationMapping().folioLocationId(randomIdAsString())
      .configurationId(randomIdAsString()), RemoteLocationConfigurationMapping.class);
    // Verify cache disable via MODRS-42
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // notNullValue());
    assertThat(delete(mappingsUrl + responseEntity.getBody()
      .getFolioLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
  }

  @Test
  void canPostExtendedRemoteLocationConfigurationMapping() {
    var responseEntity = post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping().finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
        .originalLocationId(randomIdAsString()), ExtendedRemoteLocationConfigurationMapping.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody()
      .getFinalLocationId(), notNullValue());
    assertThat(responseEntity.getBody()
      .getRemoteConfigurationId(), notNullValue());


    var mapping = get(extendedMappingsUrl + "/" + responseEntity.getBody().getFinalLocationId(),
      ExtendedRemoteLocationConfigurationMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(responseEntity.getBody(), mapping.getMappings().get(0), true, ExtendedRemoteLocationConfigurationMapping.class, METADATA));
    // Verify caching disable via MODRS-42
    // assertTrue(EqualsBuilder.reflectionEquals(
    // requireNonNull(
    // requireNonNull(cacheManager.getCache(MAPPINGS)).get(responseEntity.getBody().getFinalLocationId())).get(), mapping, true,
    // StorageConfiguration.class, METADATA));
  }

  @Test
  void shouldOverwriteRemoteConfigurationIdWhenPostOnExistingMapping() {
    var finalLocationId = randomIdAsString();
    var originalLocationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(randomIdAsString())
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var existingMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);

    var newRemoteConfigurationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(newRemoteConfigurationId)
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var overwrittenMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);
    assertEquals(newRemoteConfigurationId, overwrittenMapping.getRemoteConfigurationId());
    assertEquals(existingMapping.getOriginalLocationId(), overwrittenMapping.getOriginalLocationId());

    delete(mappingsUrl + "/" + finalLocationId);
  }

  @Test
  void shouldAddOriginalLocationIdIfNotPresentWhenPostOnExistingMapping() {
    var finalLocationId = randomIdAsString();
    var remoteConfigurationId = randomIdAsString();
    var originalLocationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var existingMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);

    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var overwrittenMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);
    assertTrue(EqualsBuilder.reflectionEquals(existingMapping, overwrittenMapping, true, ExtendedRemoteLocationConfigurationMapping.class));

    var newOriginalLocationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(newOriginalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var mappings = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class).getBody().getMappings();
    assertThat(mappings.size(), is(2));
    assertNotEquals(mappings.get(0).getOriginalLocationId(), mappings.get(1).getOriginalLocationId());
    mappings.forEach(mapping -> assertThat(mapping.getOriginalLocationId(), in(Set.of(originalLocationId, newOriginalLocationId))));

    delete(mappingsUrl + "/" + finalLocationId);
  }

  @Test
  void shouldRemoveOriginalLocationIdFromMappingWhenChangingFinalLocationId() {
    var finalLocationId = randomIdAsString();
    var remoteConfigurationId = randomIdAsString();
    var originalLocationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    var existingMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);
    assertEquals(originalLocationId, existingMapping.getOriginalLocationId());

    var newFinalLocationId = randomIdAsString();
    post(extendedMappingsUrl,
      new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(newFinalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(originalLocationId),
      ExtendedRemoteLocationConfigurationMapping.class);

    existingMapping = get(extendedMappingsUrl + "/" + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);
    assertThat(existingMapping.getOriginalLocationId(), nullValue());

    var newMapping = get(extendedMappingsUrl + "/" + newFinalLocationId, ExtendedRemoteLocationConfigurationMappings.class)
      .getBody().getMappings().get(0);
    assertEquals(originalLocationId, newMapping.getOriginalLocationId());

    delete(mappingsUrl + "/" + finalLocationId);
    delete(mappingsUrl + "/" + newFinalLocationId);
  }

  @Test
  void canGetExtendedMappingsByCriteria() {
    var orig = randomIdAsString();
    var mapping = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping()
      .finalLocationId(randomIdAsString()).remoteConfigurationId(randomIdAsString()).originalLocationId(orig),
      ExtendedRemoteLocationConfigurationMapping.class)
      .getBody();

    var response = get(extendedMappingsUrl + "?finalLocationId=" + mapping.getFinalLocationId(), ExtendedRemoteLocationConfigurationMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getMappings().get(0), true, ExtendedRemoteLocationConfigurationMapping.class));

    response = get(extendedMappingsUrl + "?remoteStorageConfigurationId=" + mapping.getRemoteConfigurationId(), ExtendedRemoteLocationConfigurationMappings.class).getBody();
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getMappings().get(0), true, ExtendedRemoteLocationConfigurationMapping.class));

    post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(mapping.getFinalLocationId()).remoteConfigurationId(mapping.getRemoteConfigurationId()).originalLocationId(randomIdAsString()),
      ExtendedRemoteLocationConfigurationMapping.class);
    response = get(extendedMappingsUrl + "?originalLocationId=" + mapping.getOriginalLocationId(), ExtendedRemoteLocationConfigurationMappings.class).getBody();
    assertThat(response.getMappings().size(), is(1));
    assertTrue(EqualsBuilder.reflectionEquals(mapping, response.getMappings().get(0), true, ExtendedRemoteLocationConfigurationMapping.class));

    delete(mappingsUrl + "/" + mapping.getFinalLocationId());
  }

  @Test
  void canGetAllExtendedRemoteLocationConfigurationMappings() {
    var mapping = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping()
      .finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
      .originalLocationId(randomIdAsString()),
      ExtendedRemoteLocationConfigurationMapping.class);
    ResponseEntity<ExtendedRemoteLocationConfigurationMappings> responseEntity = get(extendedMappingsUrl, ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertFalse(responseEntity.getBody().getMappings().isEmpty());

    delete(mappingsUrl + "/" + mapping.getBody().getFinalLocationId());
  }

  @Test
  void canGetMappingsLocations() {
    ResponseEntity<ExtendedRemoteLocationConfigurationMappings> responseEntity = get(mappingsLocationsUrl, ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(2));
    assertThat(responseEntity.getBody().getMappings().get(0).getOriginalLocationId(), notNullValue());
    assertThat(responseEntity.getBody().getMappings().get(1).getFinalLocationId(), nullValue());
  }

  @Test
  void canGetMappingsLocationsByCriteria() {
    var responseEntity = get(mappingsLocationsUrl + "?originalLocationId=fcd64ce1-6995-48f0-840e-89ffa2288371", ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(1));

    responseEntity = get(mappingsLocationsUrl + "?remoteStorageConfigurationId=de17bad7-2a30-4f1c-bee5-f653ded15629", ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(2));

    responseEntity = get(mappingsLocationsUrl + "?finalLocationId=53cf956f-c1df-410b-8bea-27f712cca7c0", ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(2));
  }

  @Test
  void canGetExtendedRemoteLocationConfigurationMappingsById() {
    var finalLocationId = randomIdAsString();
    var remoteConfigurationId = randomIdAsString();
    var mapping1 = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(randomIdAsString()),
      ExtendedRemoteLocationConfigurationMapping.class).getBody();
    var mapping2 = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping()
        .finalLocationId(finalLocationId)
        .remoteConfigurationId(remoteConfigurationId)
        .originalLocationId(randomIdAsString()),
      ExtendedRemoteLocationConfigurationMapping.class).getBody();

    var response = get(extendedMappingsUrl + finalLocationId, ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertThat(response.getBody().getMappings().size(), is(2));
    assertTrue(Arrays.asList(mapping1, mapping2).containsAll(response.getBody().getMappings()));
    // Verify cache disable via MODRS-42
    // Object cachedMapping =
    // requireNonNull(requireNonNull(cacheManager.getCache(MAPPINGS)).get(mapping.getFinalLocationId())).get();
    // assertTrue(EqualsBuilder.reflectionEquals(cachedMapping, secondResponse.getBody(), true, LocationMapping.class, "metadata"));

    delete(mappingsUrl + "/" + finalLocationId);
  }

  @Test
  void canDeleteExtendedRemoteLocationConfigurationMappingByFinalLocationId() {
    var response = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping().finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
      .originalLocationId(randomIdAsString()), ExtendedRemoteLocationConfigurationMapping.class);
    assertThat(delete(mappingsUrl + "/" + response.getBody().getFinalLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
    // Verify cache disable via MODRS-42
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // notNullValue());
    // assertThat(requireNonNull(cacheManager.getCache(MAPPINGS)).get(requireNonNull(responseEntity.getBody()).getFinalLocationId()),
    // nullValue());
  }

  @Test
  void canDeleteOriginalLocationByRemoteConfigurationAndOriginalLocationIds() {
    var mapping1 = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping().finalLocationId(randomIdAsString())
      .remoteConfigurationId(randomIdAsString())
      .originalLocationId(randomIdAsString()), ExtendedRemoteLocationConfigurationMapping.class).getBody();
    var mapping2 = post(extendedMappingsUrl, new ExtendedRemoteLocationConfigurationMapping().finalLocationId(mapping1.getFinalLocationId())
      .remoteConfigurationId(mapping1.getRemoteConfigurationId())
      .originalLocationId(randomIdAsString()), ExtendedRemoteLocationConfigurationMapping.class).getBody();
    var mappings = get(extendedMappingsUrl + "/" + mapping1.getFinalLocationId(), ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(mappings.getBody().getMappings().size(), is(2));

    assertThat(delete(extendedMappingsUrl + "/" + mapping1.getRemoteConfigurationId() + "/" + mapping2.getOriginalLocationId()).getStatusCode(), is(HttpStatus.NO_CONTENT));

    mappings = get(extendedMappingsUrl + "/" + mapping1.getFinalLocationId(), ExtendedRemoteLocationConfigurationMappings.class);
    assertThat(mappings.getBody().getMappings().size(), is(1));
    assertEquals(mapping1.getOriginalLocationId(), mappings.getBody().getMappings().get(0).getOriginalLocationId());

    delete(mappingsUrl + "/" + mapping1.getFinalLocationId());
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    var invalidBody1 = new HttpEntity(new RemoteLocationConfigurationMapping().folioLocationId("abcde")
      .configurationId("abcde"));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> post(mappingsUrl, invalidBody1, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));

    var invalidBody2 = new HttpEntity(new RemoteLocationConfigurationMapping().folioLocationId("abcde")
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
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    String urlWithRandomUuid = mappingsUrl + "abcde";
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }
}
