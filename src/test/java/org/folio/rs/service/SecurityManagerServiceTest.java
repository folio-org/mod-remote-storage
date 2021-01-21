package org.folio.rs.service;

import org.folio.rs.controller.ControllerTestBase;

import org.folio.rs.repository.CredentialsRepository;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.UUID;

import static java.util.stream.Collectors.toList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.*;

public class SecurityManagerServiceTest extends ControllerTestBase {

  @Autowired
  SecurityManagerService securityManagerService;
  @Autowired
  private CredentialsRepository credentialsRepository;
 /* @Autowired
  FolioSystemUserHolder folioContext;

  public static final String EXISTED_USER = "existed_user";
  public static final String NON_EXISTED_USER = "non_existed_user";
  public static final String NON_PRESENTED_USER = "non_presented_user";


  @BeforeEach
  public void setUp() {

    folioContext.setOkapiUrl(String.format("http://localhost:%s/", WIRE_MOCK_PORT));
    folioContext.setOkapiToken("AAA");
    folioContext.setTenant("test");

    SystemUserParameters credential1 = new SystemUserParameters();
    credential1.setId(UUID.randomUUID());
    credential1.setUsername(EXISTED_USER);
    credential1.setPassword("pwd");

    credentialsRepository.save(credential1);

    SystemUserParameters credential2 = new SystemUserParameters();
    credential2.setId(UUID.randomUUID());
    credential2.setUsername(NON_EXISTED_USER);
    credential2.setPassword("pwd");

    credentialsRepository.save(credential2);

    wireMockServer.resetAll();
  }

  @Test
  void testCreateNonExistedUser() {
    securityManagerService.createBackgroundUser(NON_EXISTED_USER, );
    List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
    assertThat(paths, Matchers.contains("/perms/users", "/authn/credentials", "/users", "/users?query=username==non_existed_user"));
  }

  @Test
  void testCreateExistedUser() {
    securityManagerService.createBackgroundUser(EXISTED_USER, );
    List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
    assertThat(paths, Matchers.contains("/perms/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496/permissions?indexField=userId",
      "/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496",
      "/users?query=username==existed_user"));
  }

  @Test
  void testCreateNonPresentedUser() {
    assertThrows(IllegalStateException.class, () -> securityManagerService.createBackgroundUser(NON_PRESENTED_USER, ));
  }

  @Test
  void testLoginBackgroundUser() {
    String apiKey = securityManagerService.loginSystemUser(EXISTED_USER);
    assertThat(apiKey, notNullValue());
  }

  @Test
  void testLoginBackgroundUserFailed() {
    assertThrows(IllegalStateException.class, () -> securityManagerService.loginSystemUser(NON_PRESENTED_USER));
  }*/

}
