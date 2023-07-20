package org.folio.rs;

import static org.folio.rs.controller.TenantController.PARAMETER_LOAD_SAMPLE;

import org.folio.rs.controller.TenantController;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import org.folio.tenant.domain.dto.Parameter;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.TestSocketUtils;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.client.RestTemplate;

import com.github.tomakehurst.wiremock.WireMockServer;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import lombok.extern.log4j.Log4j2;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.yml")
@ActiveProfiles("test")
@EnableTransactionManagement
@AutoConfigureEmbeddedDatabase(beanName = "dataSource")
@Log4j2
public class TestBase {
  public static final String METADATA = "metadata";
  private static HttpHeaders headers;
  private static RestTemplate restTemplate;
  public static WireMockServer wireMockServer;
  public static String TEST_TENANT = "test_tenant";

  @LocalServerPort
  protected int okapiPort;

  public final static int WIRE_MOCK_PORT = TestSocketUtils.findAvailableTcpPort();

  @Autowired
  private TenantController tenantController;

  @Autowired
  private FolioModuleMetadata moduleMetadata;

  @BeforeEach
  void setUp() {
    try (var context = getFolioExecutionContextSetter()) {
      tenantController.postTenant(new TenantAttributes().moduleTo("mod_remote_storage")
        .addParametersItem(new Parameter().key(PARAMETER_LOAD_SAMPLE).value("true")));
    }
  }
  public static String getOkapiUrl() {
    return String.format("http://localhost:%s", WIRE_MOCK_PORT);
  }

  @AfterEach
  void eachTearDown() {
    try (var context = getFolioExecutionContextSetter()) {
      tenantController.deleteTenant(TEST_TENANT);
      wireMockServer.resetAll();
    }
  }


  @BeforeAll
  static void testSetup() {
    headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add(XOkapiHeaders.TENANT, TEST_TENANT);
    headers.add(XOkapiHeaders.URL, getOkapiUrl());
    restTemplate = new RestTemplate();

    wireMockServer = new WireMockServer(WIRE_MOCK_PORT);
    wireMockServer.start();
  }

  @AfterAll
  static void tearDown() {
    wireMockServer.stop();
  }

  public <T> ResponseEntity<T> get(String url, Class<T> clazz) {
    return restTemplate.exchange(url, HttpMethod.GET, new HttpEntity<>(headers), clazz);
  }

  public <T> ResponseEntity<T> post(String url, Object entity, Class<T> clazz) {
    return restTemplate.exchange(url, HttpMethod.POST, new HttpEntity<>(entity, headers), clazz);
  }

  public ResponseEntity<String> put(String url, Object entity) {
    return restTemplate.exchange(url, HttpMethod.PUT, new HttpEntity<>(entity, headers), String.class);
  }

  public ResponseEntity<String> delete(String url) {
    return restTemplate.exchange(url, HttpMethod.DELETE, new HttpEntity<>(headers), String.class);
  }

  public FolioExecutionContextSetter getFolioExecutionContextSetter() {
    return new FolioExecutionContextSetter(AsyncFolioExecutionContext.builder().tenantId(TEST_TENANT).moduleMetadata(moduleMetadata).okapiUrl(getOkapiUrl()).build());
  }
}
