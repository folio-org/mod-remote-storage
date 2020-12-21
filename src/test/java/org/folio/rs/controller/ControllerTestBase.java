package org.folio.rs.controller;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.properties")
@ActiveProfiles("TestDB")
@AutoConfigureEmbeddedDatabase(beanName = "dataSource")
class ControllerTestBase {
  protected static HttpHeaders headers;
  protected static RestTemplate restTemplate;

  @LocalServerPort
  protected int port;

  @BeforeAll
  static void testSetup() {
    headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add(XOkapiHeaders.TENANT, "test_tenant");
    restTemplate = new RestTemplate();
  }
}
