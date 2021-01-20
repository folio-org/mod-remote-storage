package org.folio.rs.controller;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.UUID;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.domain.dto.TimeUnits;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;

public class ConfigurationsControllerTest extends ControllerTestBase {

  private static final String CONFIGURATIONS_URL = "http://localhost:%s/remote-storage/configurations/";
  private static final String PROVIDERS_URL = "http://localhost:%s/remote-storage/providers/";
  private static final String TENANT_URL = "http://localhost:%s/_/tenant";

  private String configurationsUrl;

  @Autowired
  private CacheManager cacheManager;

  @BeforeEach
  void prepareUrl() {
    configurationsUrl = String.format(CONFIGURATIONS_URL, okapiPort);
  }

  @AfterEach
  void clearCache() {
    ofNullable(cacheManager.getCache("configurations")).ifPresent(Cache::clear);
  }

  @Test
  void canPostTenantWithParameters() {
    String tenants = "{\"module_to\":\"moduleId\", \"parameters\": [ { \"key\":\"loadSample\", \"value\": true } ] }";
    ResponseEntity<String> response = restTemplate
      .exchange(String.format(TENANT_URL, okapiPort), HttpMethod.POST, new HttpEntity<>(tenants, headers), String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPostTenantWithoutParameters() {
    String tenants = "{\"module_to\":\"moduleId\"}";
    ResponseEntity<String> response = restTemplate
      .exchange(String.format(TENANT_URL, okapiPort), HttpMethod.POST, new HttpEntity<>(tenants, headers), String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPostConfiguration() {
    ResponseEntity<StorageConfiguration> responseEntity = restTemplate
      .postForEntity(configurationsUrl, buildConfiguration(null), StorageConfiguration.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody().getId(), notNullValue());
    assertThat(responseEntity.getBody().getMetadata().getCreatedDate(), notNullValue());
    assertThat(fetchConfigurations().getTotalRecords(), is(2));
  }

  @Test
  void canGetAllConfigurations() {
    ResponseEntity<StorageConfigurations> responseEntity = restTemplate
      .getForEntity(configurationsUrl, StorageConfigurations.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody().getTotalRecords(), is(1));
  }

  @Test
  void canGetConfigurationById() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations().get(0);
    ResponseEntity<StorageConfiguration> response = restTemplate
      .getForEntity(configurationsUrl + configurationDto.getId(), StorageConfiguration.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPutConfiguration() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations().get(0);
    configurationDto.accessionDelay(5).accessionTimeUnit(TimeUnits.MINUTES);
    ResponseEntity<String> response = restTemplate.exchange(configurationsUrl + configurationDto.getId(),
      HttpMethod.PUT, new HttpEntity<>(configurationDto), String.class);
    assertThat(response.getStatusCode(), equalTo(HttpStatus.NO_CONTENT));
  }

  @Test
  void canDeleteConfiguration() {
    StorageConfiguration configuration = fetchConfigurations().getConfigurations().get(0);
    assertThat(restTemplate.exchange(configurationsUrl + configuration.getId(), HttpMethod.DELETE,
      new HttpEntity<>(null), String.class).getStatusCode(), is(HttpStatus.NO_CONTENT));
    assertThat(fetchConfigurations().getTotalRecords(), is(0));
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    HttpEntity entityMissingName = new HttpEntity(new StorageConfiguration().name(null));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .postForEntity(configurationsUrl, entityMissingName, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnUnprocessableEntityOnDuplicatedConfigurationName() {
    StorageConfiguration initialEntity = buildConfiguration(null).name("RS");
    ResponseEntity<StorageConfiguration> responseEntity = restTemplate
      .postForEntity(configurationsUrl, initialEntity, StorageConfiguration.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
      () -> restTemplate.postForEntity(configurationsUrl, initialEntity, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .delete(configurationsUrl + "abcde"));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));

    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .getForObject(configurationsUrl + "abcde", String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldReturnNotFoundForWrongUuid() {
    String randomUuid = randomIdAsString();
    String urlWithRandomUuid = configurationsUrl + randomUuid;
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .getForObject(urlWithRandomUuid, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    HttpEntity entity = new HttpEntity<>(buildConfiguration(randomUuid));
    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate.put(urlWithRandomUuid, entity));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldReturnBadRequestForIdsMismatch() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations().get(0)
      .accessionDelay(5).accessionTimeUnit(TimeUnits.MINUTES);
    String urlWithAnotherUuid = configurationsUrl + randomIdAsString();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .put(urlWithAnotherUuid, configurationDto, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldReturnAllProviders() {
    ResponseEntity<List> response = restTemplate.getForEntity(String.format(PROVIDERS_URL, okapiPort), List.class);
    assertEquals(2, requireNonNull(response.getBody()).size());
  }

  private StorageConfiguration buildConfiguration(String id) {
    return new StorageConfiguration()
      .id(id)
      .name("RS1")
      .providerName("Dematic")
      .url("https://rs1.dematic.com")
      .accessionDelay(2)
      .accessionTimeUnit(TimeUnits.MINUTES);
  }

  private StorageConfigurations fetchConfigurations() {
    return restTemplate.getForObject(configurationsUrl, StorageConfigurations.class);
  }
}
