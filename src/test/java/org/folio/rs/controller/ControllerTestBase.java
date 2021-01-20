package org.folio.rs.controller;

import com.github.tomakehurst.wiremock.WireMockServer;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
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
@Sql(executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:PopulateTestData.sql")
@Sql(executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:ClearTestData.sql")
public class ControllerTestBase {
  protected static HttpHeaders headers;
  protected static RestTemplate restTemplate;
  public static WireMockServer wireMockServer;

  @LocalServerPort
  protected int okapiPort;

  public final static int WIRE_MOCK_PORT = SocketUtils.findAvailableTcpPort();

  @BeforeAll
  static void testSetup() {
    headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add(XOkapiHeaders.TENANT, "test_tenant");
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
