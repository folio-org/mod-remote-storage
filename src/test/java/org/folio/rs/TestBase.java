package org.folio.rs;

import com.github.tomakehurst.wiremock.WireMockServer;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.controller.TenantController;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import org.folio.tenant.domain.dto.Parameter;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.util.SocketUtils;
import org.springframework.web.client.RestTemplate;

import static org.folio.rs.controller.TenantController.PARAMETER_LOAD_SAMPLE;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.properties")
@ActiveProfiles("TestDB")
@EnableTransactionManagement
@AutoConfigureEmbeddedDatabase(beanName = "testDataSource")
@Log4j2
public class TestBase {
  protected static HttpHeaders headers;
  protected static RestTemplate restTemplate;
  public static WireMockServer wireMockServer;
  public static String TEST_TENANT = "test_tenant";

  @LocalServerPort
  protected int okapiPort;

  public final static int WIRE_MOCK_PORT = SocketUtils.findAvailableTcpPort();

  @Autowired
  private TenantController tenantController;

  @Autowired
  private FolioModuleMetadata moduleMetadata;

  @BeforeEach
  void setUp() {
      FolioExecutionScopeExecutionContextManager.beginFolioExecutionContext(
        AsyncFolioExecutionContext.builder()
          .tenantId(TEST_TENANT)
          .moduleMetadata(moduleMetadata)
          .okapiUrl(getOkapiUrl()).build());
      tenantController.postTenant(new TenantAttributes().moduleTo("remote-storage-module")
        .addParametersItem(new Parameter().key(PARAMETER_LOAD_SAMPLE).value("true")));

      wireMockServer.resetAll();
  }

  public static String getOkapiUrl() {
    return String.format("http://localhost:%s/", WIRE_MOCK_PORT);
  }

  @AfterEach
  void eachTearDown() {
    tenantController.deleteTenant();
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
}
