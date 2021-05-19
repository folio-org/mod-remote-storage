package org.folio.rs.service;

import static java.util.stream.Collectors.toList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;

import java.util.List;
import java.util.UUID;

import org.folio.rs.TestBase;
import org.folio.rs.domain.entity.SystemUserParameters;
import org.folio.rs.repository.SystemUserParametersRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class SecurityManagerServiceTest extends TestBase {

  private static final String PASSWORD = "PWD";

  @Autowired
  SecurityManagerService securityManagerService;

  @Autowired
  private SystemUserParametersRepository credentialsRepository;

  public static final String EXISTED_USER = "existed_user";
  public static final String NON_EXISTED_USER = "non_existed_user";
  public static final String NON_PRESENTED_USER = "non_presented_user";

  @Test
  void testCreateDefaultSystemUser() {
    securityManagerService.prepareSystemUser(NON_PRESENTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
    List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
    assertThat(paths, hasItems("/authn/login", "/perms/users", "/authn/credentials", "/users", "/users?query=username==non_presented_user"));
  }

  @Test
  void testCreateNonExistedUser() {
    credentialsRepository.save(buildSystemUserParameters(NON_EXISTED_USER));
    securityManagerService.prepareSystemUser(NON_EXISTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
    List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
    assertThat(paths, hasItems(  "/authn/login", "/perms/users", "/authn/credentials", "/users", "/users?query=username==non_existed_user"));
  }

  @Test
  void testCreateExistedUser() {
    credentialsRepository.save(buildSystemUserParameters(EXISTED_USER));
    securityManagerService.prepareSystemUser(EXISTED_USER, PASSWORD, getOkapiUrl(), TEST_TENANT);
    List<String> paths = wireMockServer.getAllServeEvents().stream().map(e -> e.getRequest().getUrl()).collect(toList());
    assertThat(paths, hasItems("/authn/login", "/perms/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496/permissions?indexField=userId",
      "/users/c78aa9ec-b7d3-4d53-9e43-20296f39b496",
      "/users?query=username==existed_user"));
  }

  private SystemUserParameters buildSystemUserParameters(String username) {
    SystemUserParameters systemUserParameters = new SystemUserParameters();
    systemUserParameters.setId(UUID.randomUUID());
    systemUserParameters.setUsername(username);
    systemUserParameters.setTenantId(TEST_TENANT);
    systemUserParameters.setOkapiUrl(getOkapiUrl());
    systemUserParameters.setPassword(PASSWORD);
    return systemUserParameters;
  }
}
