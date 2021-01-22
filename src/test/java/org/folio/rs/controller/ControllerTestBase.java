package org.folio.rs.controller;

import com.github.tomakehurst.wiremock.WireMockServer;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.util.SocketUtils;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.properties")
@ActiveProfiles("TestDB")
@AutoConfigureEmbeddedDatabase(beanName = "dataSource")
public class ControllerTestBase {
  protected static HttpHeaders headers;
  protected static RestTemplate restTemplate;
  public static WireMockServer wireMockServer;
  public static String TEST_TENANT = "test_tenant";

  @Autowired
  private TenantController tenantController;

  @LocalServerPort
  protected int okapiPort;

  public final static int WIRE_MOCK_PORT = SocketUtils.findAvailableTcpPort();

   @BeforeEach
  void prepareDB() {
   tenantController.postTenant(new TenantAttributes().moduleTo("remote-storage-module"));
  }


  @BeforeAll
  static void testSetup() {
    headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add(XOkapiHeaders.TENANT, TEST_TENANT);
    headers.add(XOkapiHeaders.URL, String.format("http://localhost:%s/", WIRE_MOCK_PORT));
    restTemplate = new RestTemplate();

    wireMockServer = new WireMockServer(WIRE_MOCK_PORT);
    wireMockServer.start();

  }

  @AfterAll
  static void tearDown() {
    wireMockServer.stop();
  }
}
