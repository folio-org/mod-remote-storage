package org.folio.rs.client;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.okJson;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static org.folio.rs.support.wiremock.WiremockContainerExtension.getWireMockAdminClient;
import static org.folio.rs.support.wiremock.WiremockContainerExtension.getWireMockUrl;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.github.tomakehurst.wiremock.http.Body;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import lombok.extern.log4j.Log4j2;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rs.support.wiremock.EnableWiremock;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.config.properties.FolioEnvironment;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.testing.extension.EnablePostgres;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@Log4j2
@EnableWiremock
@EnablePostgres
@ActiveProfiles("test")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class RemoteStorageUsersClientTest {

  @Autowired private FolioEnvironment folioEnvironment;
  @Autowired private FolioModuleMetadata moduleMetadata;
  @Autowired private RemoteStorageUsersClient usersClient;

  @BeforeEach
  void beforeEach() {
    folioEnvironment.setOkapiUrl(getWireMockUrl());
  }

  @Test
  void getUser_found() {
    getWireMockAdminClient().addStubMapping(
      get(urlPathEqualTo("/users/123"))
        .willReturn(okJson("""
          { "username": "onetwothree" }
          """))
        .build());
    try (var ignored = getFolioExecutionContextSetter()) {
      var user = usersClient.getUser("123");
      assertThat(user.isPresent(), is(true));
      assertThat(user.get().getUsername(), is("onetwothree"));
    }
  }

  @Test
  void getUser_notFound() {
    getWireMockAdminClient().addStubMapping(
      get(urlPathEqualTo("/users/456")).willReturn(aResponse()
          .withStatus(404)
          .withHeader("Content-Type", "text/plain")
          .withResponseBody(new Body("Not Found")))
        .build());

    try (var ignored = getFolioExecutionContextSetter()) {
      assertNull(usersClient.getUser("456").orElse(null));
    }
  }

  public FolioExecutionContextSetter getFolioExecutionContextSetter() {
    var headers = Map.<String, Collection<String>>of(
      XOkapiHeaders.TENANT.toLowerCase(), List.of("rainbow"),
      XOkapiHeaders.URL.toLowerCase(), List.of(getWireMockUrl()));
    var context = new DefaultFolioExecutionContext(moduleMetadata, headers);
    return new FolioExecutionContextSetter(context);
  }
}
