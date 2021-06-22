package org.folio.rs.controller;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;
import static org.folio.rs.domain.dto.ReturningWorkflowDetails.CAIASOFT;
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
import org.folio.rs.domain.dto.AccessionWorkflowDetails;
import org.folio.rs.domain.dto.ReturningWorkflowDetails;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.domain.dto.TimeUnits;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;

public class ConfigurationsTest extends TestBase {

  private static final String CONFIGURATIONS_URL = "http://localhost:%s/remote-storage/configurations/";
  private static final String TENANT_URL = "http://localhost:%s/_/tenant";

  private String configurationsUrl;

  @Autowired
  private CacheManager cacheManager;

  @BeforeEach
  void prepareUrl() {
    configurationsUrl = String.format(CONFIGURATIONS_URL, okapiPort);
    ofNullable(cacheManager.getCache("configurations")).ifPresent(Cache::clear);
  }

  @Test
  void canPostTenantWithParameters() {
    String tenants = "{\"module_to\":\"moduleId\", \"parameters\": [ { \"key\":\"loadSample\", \"value\": true } ] }";
    ResponseEntity<String> response = post(String.format(TENANT_URL, okapiPort), tenants, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPostTenantWithoutParameters() {
    String tenants = "{\"module_to\":\"moduleId\"}";
    ResponseEntity<String> response = post(String.format(TENANT_URL, okapiPort), tenants, String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPostConfiguration() {
    ResponseEntity<StorageConfiguration> responseEntity = post(configurationsUrl, buildConfiguration(null),
        StorageConfiguration.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    assertThat(responseEntity.getBody()
      .getId(), notNullValue());
    assertThat(responseEntity.getBody()
      .getMetadata()
      .getCreatedDate(), notNullValue());
    assertThat(fetchConfigurations().getTotalRecords(), is(3));

    // Verify caching disable via MODRS-42
    StorageConfiguration configuration = get(configurationsUrl + "/" + responseEntity.getBody()
      .getId(), StorageConfiguration.class).getBody();
    assertTrue(
        EqualsBuilder.reflectionEquals(responseEntity.getBody(), configuration, true, StorageConfiguration.class, "metadata"));
    // assertTrue(EqualsBuilder.reflectionEquals(
    // requireNonNull(
    // requireNonNull(cacheManager.getCache("configurations")).get(responseEntity.getBody().getId())).get(), configuration, true,
    // StorageConfiguration.class, "metadata"));
  }

  @Test
  void canPostAndPutConfigurationWithReturningWorkflow() {
    var requestBody = "{\"name\":\"RemoteStorage\", \"providerName\": \"CaiaSoft\", \"accessionTimeUnit\":\"minutes\", \"returningWorkflowDetails\":\"Scanned to CaiaSoft\"}";
    var configuration = post(configurationsUrl, requestBody, StorageConfiguration.class).getBody();
    assertThat(configuration.getReturningWorkflowDetails(), is(ReturningWorkflowDetails.CAIASOFT));
    configuration.returningWorkflowDetails(ReturningWorkflowDetails.FOLIO);
    put(configurationsUrl + configuration.getId(), configuration).getBody();
    var updatedConfiguration = get(configurationsUrl + configuration.getId(), StorageConfiguration.class).getBody();
    assertThat(updatedConfiguration.getReturningWorkflowDetails(), is(ReturningWorkflowDetails.FOLIO));
    delete(configurationsUrl + configuration.getId());
  }

  @Test
  void canPostAndPutConfigurationWithAccessionWorkflow() {
    var requestBody = "{\"name\":\"CaiaSoft\", \"providerName\":\"CaiaSoft\", \"accessionTimeUnit\":\"minutes\", \"accessionWorkflowDetails\":\"Duplicate holdings\"}";
    var configuration = post(configurationsUrl, requestBody, StorageConfiguration.class).getBody();
    assertThat(configuration.getAccessionWorkflowDetails(), is(AccessionWorkflowDetails.DUPLICATE_HOLDINGS));
    configuration.accessionWorkflowDetails(AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION);
    put(configurationsUrl + configuration.getId(), configuration).getBody();
    var updatedConfiguration = get(configurationsUrl + configuration.getId(), StorageConfiguration.class).getBody();
    assertThat(updatedConfiguration.getAccessionWorkflowDetails(), is(AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION));
    delete(configurationsUrl + configuration.getId());
  }

  @Test
  void canGetAllConfigurations() {
    ResponseEntity<StorageConfigurations> responseEntity = get(configurationsUrl, StorageConfigurations.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
    assertThat(responseEntity.getBody()
      .getTotalRecords(), is(2));
  }

  @Test
  void canGetConfigurationById() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations()
      .get(0);
    assertThat(requireNonNull(cacheManager.getCache("configurations")).get(configurationDto.getId()), nullValue());
    ResponseEntity<StorageConfiguration> firstResponse = get(configurationsUrl + configurationDto.getId(),
        StorageConfiguration.class);
    assertThat(firstResponse.getStatusCode(), is(HttpStatus.OK));

    // Verify cache disable via MODRS-42
    // Object cachedConfiguration =
    // requireNonNull(requireNonNull(cacheManager.getCache("configurations")).get(configurationDto.getId())).get();

    ResponseEntity<StorageConfiguration> secondResponse = get(configurationsUrl + configurationDto.getId(),
        StorageConfiguration.class);
    assertThat(secondResponse.getStatusCode(), is(HttpStatus.OK));
    assertTrue(
        EqualsBuilder.reflectionEquals(configurationDto, secondResponse.getBody(), true, StorageConfiguration.class, "metadata"));
    // assertTrue(EqualsBuilder.reflectionEquals(cachedConfiguration, secondResponse.getBody(), true, StorageConfiguration.class,
    // "metadata"));

  }

  @Test
  void canPutConfiguration() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations()
      .get(0);
    configurationDto.accessionDelay(5)
      .accessionTimeUnit(TimeUnits.MINUTES);
    ResponseEntity<String> response = put(configurationsUrl + configurationDto.getId(), configurationDto);
    assertThat(response.getStatusCode(), equalTo(HttpStatus.NO_CONTENT));

    // Verify caching disable via MODRS-42
    StorageConfiguration configuration = get(configurationsUrl + "/" + configurationDto.getId(), StorageConfiguration.class)
      .getBody();
    assertTrue(EqualsBuilder.reflectionEquals(configurationDto, configuration, true, StorageConfiguration.class, "metadata"));
    // assertTrue(EqualsBuilder.reflectionEquals(requireNonNull(requireNonNull(cacheManager.getCache("configurations")).get(configurationDto.getId())).get(),
    // configuration, true, StorageConfiguration.class, "metadata"));

  }

  @Test
  void canDeleteConfiguration() {
    StorageConfiguration configuration = fetchConfigurations().getConfigurations()
      .get(0);
    int size = fetchConfigurations().getConfigurations().size();
    requireNonNull(cacheManager.getCache("configurations")).put(configuration.getId(), configuration);
    assertThat(delete(configurationsUrl + configuration.getId()).getStatusCode(), is(HttpStatus.NO_CONTENT));
    assertThat(fetchConfigurations().getTotalRecords(), is(size - 1));
    assertThat(requireNonNull(cacheManager.getCache("configurations")).get(configuration.getId()), nullValue());
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    HttpEntity entityMissingName = new HttpEntity(new StorageConfiguration().name(null));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> post(configurationsUrl, entityMissingName, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnUnprocessableEntityOnDuplicatedConfigurationName() {
    StorageConfiguration initialEntity = buildConfiguration(null).name("RS");
    ResponseEntity<StorageConfiguration> responseEntity = post(configurationsUrl, initialEntity, StorageConfiguration.class);
    assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> post(configurationsUrl, initialEntity, StorageConfiguration.class));
    assertThat(exception.getStatusCode(), is(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(configurationsUrl + "abcde"));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));

    exception = assertThrows(HttpClientErrorException.class, () -> get(configurationsUrl + "abcde", String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldReturnNotFoundForWrongUuid() {
    String randomUuid = randomIdAsString();
    String urlWithRandomUuid = configurationsUrl + randomUuid;
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> delete(urlWithRandomUuid));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    exception = assertThrows(HttpClientErrorException.class, () -> get(urlWithRandomUuid, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    var config = buildConfiguration(randomUuid);
    exception = assertThrows(HttpClientErrorException.class, () -> put(urlWithRandomUuid, config));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldReturnBadRequestForIdsMismatch() {
    StorageConfiguration configurationDto = fetchConfigurations().getConfigurations()
      .get(0)
      .accessionDelay(5)
      .accessionTimeUnit(TimeUnits.MINUTES);
    String urlWithAnotherUuid = configurationsUrl + randomIdAsString();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> put(urlWithAnotherUuid, configurationDto));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  private StorageConfiguration buildConfiguration(String id) {
    return new StorageConfiguration().id(id)
      .name("Remote Storage")
      .apiKey("i+X9dfNOztkBfoAmGTXf/w==")
      .providerName("Dematic")
      .url("https://rs.dematic.com")
      .accessionDelay(2)
      .accessionTimeUnit(TimeUnits.MINUTES);
  }

  private StorageConfigurations fetchConfigurations() {
    return get(configurationsUrl, StorageConfigurations.class).getBody();
  }
}
