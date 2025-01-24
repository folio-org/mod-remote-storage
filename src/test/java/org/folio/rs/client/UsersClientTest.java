package org.folio.rs.client;

import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.okJson;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.tomakehurst.wiremock.junit5.WireMockRuntimeInfo;
import com.github.tomakehurst.wiremock.junit5.WireMockTest;
import feign.FeignException;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.domain.dto.User;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.config.properties.FolioEnvironment;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ExtendWith(SpringExtension.class)
@TestPropertySource("classpath:application-test.yml")
@ActiveProfiles("test")
@EnableTransactionManagement
@AutoConfigureEmbeddedDatabase(beanName = "dataSource")
@Log4j2
@WireMockTest
class UsersClientTest {
  @MockBean
  private FolioModuleMetadata moduleMetadata;
  @Autowired
  private FolioEnvironment folioEnvironment;
  @Autowired
  private UsersClient usersClient;
  private String okapiUrl;

  @BeforeEach
  void beforeEach(WireMockRuntimeInfo wireMockRuntimeInfo) {
    okapiUrl = "http://localhost:" + wireMockRuntimeInfo.getHttpPort();
    folioEnvironment.setOkapiUrl(okapiUrl);
  }

  @Test
  void getUser_found() {
    stubFor(get("/users/123").willReturn(okJson("""
        { "username": "onetwothree" }
        """)));
    try (var context = getFolioExecutionContextSetter()) {
      User user = usersClient.getUser("123");
      assertThat(user.getUsername(), is("onetwothree"));
    }
  }

  @Test
  void getUser_notFound() {
    try (var context = getFolioExecutionContextSetter()) {
      assertThrows(FeignException.NotFound.class, () -> usersClient.getUser("456"));
    }
  }

  public FolioExecutionContextSetter getFolioExecutionContextSetter() {
    return new FolioExecutionContextSetter(AsyncFolioExecutionContext.builder()
        .tenantId("rainbow")
        .moduleMetadata(moduleMetadata)
        .okapiUrl(okapiUrl)
        .build());
  }
}
