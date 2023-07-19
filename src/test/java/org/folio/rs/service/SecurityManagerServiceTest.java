package org.folio.rs.service;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.deleteRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static java.util.stream.Collectors.toList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.tomakehurst.wiremock.client.WireMock;
import java.util.List;

import org.folio.rs.TestBase;
import org.folio.spring.FolioModuleMetadata;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

public class SecurityManagerServiceTest extends TestBase {

  private static final String PASSWORD = "PWD";

  @Autowired
  SecurityManagerService securityManagerService;

  @Autowired
  private FolioModuleMetadata moduleMetadata;

  public static final String EXISTED_USER = "existed_user";
  public static final String NON_EXISTED_USER = "non_existed_user";
  public static final String NON_PRESENTED_USER = "non_presented_user";

  @Value("${remote-storage.system-user.username}")
  private String username;

  @Value("${remote-storage.system-user.password}")
  private String password;
  @Test
  void testCreateDefaultSystemUser() {
    try (var context = getFolioExecutionContextSetter()) {
      securityManagerService.prepareOrUpdateSystemUser(NON_PRESENTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
      List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
      assertThat(paths, hasItems("/authn/login", "/perms/users", "/authn/credentials", "/users", "/users?query=username%3D%3Dnon_presented_user"));
    }
  }

  @Test
  void testOverrideDefaultSystemUser() throws NoSuchFieldException, IllegalAccessException {
    try (var context = getFolioExecutionContextSetter()) {
      var originalSystemUserParameters = securityManagerService.getSystemUserParameters(TEST_TENANT);

      final var newOkapiUrl = "http://new-okapi-url";
      securityManagerService.prepareOrUpdateSystemUser(username, password, newOkapiUrl, TEST_TENANT);
      var updatedSystemUserParameters = securityManagerService.getSystemUserParameters(TEST_TENANT);

      assertEquals(originalSystemUserParameters.getId(), updatedSystemUserParameters.getId());
      assertEquals(originalSystemUserParameters.getUsername(), updatedSystemUserParameters.getUsername());
      assertEquals(originalSystemUserParameters.getTenantId(), updatedSystemUserParameters.getTenantId());
      assertEquals(originalSystemUserParameters.getPassword(), updatedSystemUserParameters.getPassword());
      assertEquals(originalSystemUserParameters.getOkapiToken(), updatedSystemUserParameters.getOkapiToken());
      assertThat(updatedSystemUserParameters.getOkapiUrl(), equalTo(newOkapiUrl));
    }
   }

  @Test
  void testCreateNonExistedUser() {
    try (var context = getFolioExecutionContextSetter()) {
      securityManagerService.prepareOrUpdateSystemUser(NON_EXISTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
      List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
      assertThat(paths, hasItems(  "/authn/login", "/perms/users", "/authn/credentials", "/users", "/users?query=username%3D%3Dnon_existed_user"));
    }
  }

  @Test
  void testCreateExistedUser() {
    try (var context = getFolioExecutionContextSetter()) {
      securityManagerService.prepareOrUpdateSystemUser(EXISTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
      List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
      assertThat(paths, hasItems("/authn/login", "/perms/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496/permissions?indexField=userId",
        "/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496",
        "/users?query=username%3D%3Dexisted_user"));
    }
  }

  @Test
  void testRefreshSystemUserApiKey() {
    try (var context = getFolioExecutionContextSetter()) {
      securityManagerService.refreshSystemUserApiKey(EXISTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
      List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
      assertThat(paths, hasItems("/authn/login"));
    }
  }

  @Test
  @DisplayName("Update user without previous password")
  void prepareOrUpdateSystemUserWithoutPreviousPassword() {
    wireMockServer.stubFor(
      WireMock.delete(urlEqualTo("/authn/credentials?userId=c78aa9ec-b7d3-4d53-9e43-20296f39b496"))
        .willReturn(
          aResponse()
            .withStatus(HttpStatus.NOT_FOUND.value())));
    try (var context = getFolioExecutionContextSetter()) {
      final var newOkapiUrl = "http://new-okapi-url";
      securityManagerService.prepareOrUpdateSystemUser(EXISTED_USER, password, newOkapiUrl, TEST_TENANT);
    }
    wireMockServer.verify(
      deleteRequestedFor(urlEqualTo("/authn/credentials?userId=c78aa9ec-b7d3-4d53-9e43-20296f39b496")));
    wireMockServer.verify(
      postRequestedFor(urlEqualTo("/authn/credentials")));
  }
}
