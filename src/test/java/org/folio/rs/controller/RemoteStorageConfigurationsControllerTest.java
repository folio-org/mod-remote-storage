package org.folio.rs.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.folio.rs.domain.dto.RemoteStorageConfig;
import org.folio.rs.domain.dto.RemoteStorageConfigCollection;
import org.folio.rs.domain.dto.TimeUnits;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;
import java.util.UUID;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.properties")
@ActiveProfiles("TestDB")
@AutoConfigureEmbeddedDatabase
@Sql(executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:PopulateTestData.sql")
@Sql(executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:ClearTestData.sql")
class RemoteStorageConfigurationsControllerTest {

  private static final String CONFIGURATIONS_URL = "http://localhost:%s/remote-storages/configurations/";
  private static final String TENANT_URL = "http://localhost:%s/_/tenant";

  private static HttpHeaders headers;
  private static RestTemplate restTemplate;
  private String configurationsUrl;

  @LocalServerPort
  private int port;

  @BeforeAll
  static void globalSetup() {
    headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add(XOkapiHeaders.TENANT, "test_tenant");
    restTemplate = new RestTemplate();
  }

  @BeforeEach
  void testSetup() {
    configurationsUrl = String.format(CONFIGURATIONS_URL, port);
  }

  @Test
  void postTenant() {
    String tenants = "{\"module_to\":\"moduleId\", \"parameters\": [ { \"key\":\"loadSample\", \"value\": true } ] }";
    ResponseEntity<String> response = restTemplate
      .exchange(String.format(TENANT_URL, port), HttpMethod.POST, createRequestBody(tenants), String.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPostConfiguration() {
    RemoteStorageConfig responseConfig = restTemplate.postForObject(configurationsUrl,
      createRequestBody(buildConfiguration(null)), RemoteStorageConfig.class);
    assertThat(responseConfig.getId(), notNullValue());
    assertThat(responseConfig.getMetadata().getCreatedDate(), notNullValue());
    assertThat(getRemoteStorageConfigs().getTotalRecords(), is(2));
  }

  @Test
  void canGetAllConfigurations() {
    RemoteStorageConfigCollection remoteConfigCollection = getRemoteStorageConfigs();
    assertThat(remoteConfigCollection.getTotalRecords(), is(1));
  }

  @Test
  void canGetConfigurationById() {
    RemoteStorageConfig remoteConfig = getRemoteStorageConfigs().getConfigurations().get(0);
    ResponseEntity<RemoteStorageConfig> response = restTemplate.exchange(configurationsUrl + remoteConfig.getId(),
      HttpMethod.GET, createRequestBody(), RemoteStorageConfig.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
  }

  @Test
  void canPutConfiguration() {
    RemoteStorageConfig remoteConfig = getRemoteStorageConfigs().getConfigurations().get(0);
    assertThat(remoteConfig.getAccessionDelay(), is(1));
    assertThat(remoteConfig.getAccessionTimeUnit(), is(TimeUnits.HOURS));

    remoteConfig.accessionDelay(5).accessionTimeUnit(TimeUnits.MINUTES);
    ResponseEntity<RemoteStorageConfig> response = restTemplate.exchange(configurationsUrl, HttpMethod.PUT,
      createRequestBody(remoteConfig), RemoteStorageConfig.class);
    assertThat(response.getStatusCode(), equalTo(HttpStatus.OK));
    assertThat(response.getBody().getAccessionDelay(), is(5));
    assertThat(response.getBody().getAccessionTimeUnit(), is(TimeUnits.MINUTES));
    assertThat(response.getBody().getMetadata().getUpdatedDate(), notNullValue());
  }

  @Test
  void shouldCreateConfigurationWhenIdIsNull() {
    RemoteStorageConfig remoteConfig = getRemoteStorageConfigs().getConfigurations().get(0);
    remoteConfig.id(null).accessionDelay(5).accessionTimeUnit(TimeUnits.MINUTES);

    ResponseEntity<RemoteStorageConfig> response = restTemplate.exchange(configurationsUrl, HttpMethod.PUT,
      createRequestBody(remoteConfig), RemoteStorageConfig.class);
    assertThat(response.getStatusCode(), is(HttpStatus.OK));
    assertThat(response.getBody().getId(), notNullValue());
    assertThat(response.getBody().getMetadata().getCreatedDate(), notNullValue());
    assertThat(response.getBody().getAccessionDelay(), is(5));
    assertThat(response.getBody().getAccessionTimeUnit(), is(TimeUnits.MINUTES));

    RemoteStorageConfigCollection remoteConfigCollection = getRemoteStorageConfigs();
    assertThat(remoteConfigCollection.getTotalRecords(), is(2));
  }

  @Test
  void canDeleteConfiguration() {
    RemoteStorageConfig remoteConfig = getRemoteStorageConfigs().getConfigurations().get(0);
    assertThat(restTemplate.exchange(configurationsUrl + remoteConfig.getId(), HttpMethod.DELETE, createRequestBody(),
      String.class).getStatusCode(), is(HttpStatus.NO_CONTENT));
    assertThat(getRemoteStorageConfigs().getTotalRecords(), is(0));
  }

  @Test
  void shouldReturnUnprocessableEntityForInvalidBody() {
    HttpEntity entity1 = createRequestBody(new RemoteStorageConfig().name(null));
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl, HttpMethod.POST, entity1, RemoteStorageConfig.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.UNPROCESSABLE_ENTITY));

    HttpEntity entity2 = createRequestBody(buildConfiguration("abcde"));
    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl, HttpMethod.PUT, entity2, RemoteStorageConfig.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.UNPROCESSABLE_ENTITY));
  }

  @Test
  void shouldReturnBadRequestForInvalidUuid() {
    HttpEntity entity = createRequestBody();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl + "abcde", HttpMethod.DELETE, entity, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));

    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl + "abcde", HttpMethod.GET, entity, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldReturnNotFoundForWrongUuid() {
    HttpEntity entity1 = createRequestBody();
    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl + UUID.randomUUID().toString(), HttpMethod.DELETE, entity1, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate
      .exchange(configurationsUrl + UUID.randomUUID().toString(), HttpMethod.GET, entity1, String.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));

    HttpEntity entity2 = createRequestBody(buildConfiguration(UUID.randomUUID().toString()));
    exception = assertThrows(HttpClientErrorException.class, () -> restTemplate.exchange(configurationsUrl,
      HttpMethod.PUT, entity2, RemoteStorageConfig.class));
    assertThat(exception.getStatusCode(), equalTo(HttpStatus.NOT_FOUND));
  }

  private RemoteStorageConfig buildConfiguration(String id) {
    return new RemoteStorageConfig()
      .id(id)
      .name("RS1")
      .providerName("Dematic")
      .url("https://rs1.dematic.com")
      .accessionDelay(2)
      .accessionTimeUnit(TimeUnits.MINUTES);
  }

  private RemoteStorageConfigCollection getRemoteStorageConfigs() {
    return restTemplate.exchange(configurationsUrl, HttpMethod.GET, createRequestBody(), RemoteStorageConfigCollection.class).getBody();
  }

  private HttpEntity createRequestBody() {
    return createRequestBody(null);
  }

  private HttpEntity createRequestBody(Object o) {
    return Objects.isNull(o) ? new HttpEntity(headers) : new HttpEntity(o, headers);
  }
}
