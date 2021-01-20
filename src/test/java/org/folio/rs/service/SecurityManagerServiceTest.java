package org.folio.rs.service;

import org.folio.rs.controller.ControllerTestBase;
import org.folio.rs.domain.entity.Credential;
import org.folio.rs.domain.entity.FolioContext;
import org.folio.rs.repository.CredentialsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.*;

public class SecurityManagerServiceTest extends ControllerTestBase {

  @Autowired
  SecurityManagerService securityManagerService;
  @Autowired
  private CredentialsRepository credentialsRepository;
  @Autowired
  FolioContext folioContext;

  public static final String EXISTED_USER = "existed_user";
  public static final String NON_EXISTED_USER = "non_existed_user";
  public static final String NON_PRESENTED_USER = "non_presented_user";


  @BeforeEach
  public void setUp() {

    folioContext.setOkapiUrl(String.format("http://localhost:%s/", WIRE_MOCK_PORT));
    folioContext.setOkapiToken("AAA");
    folioContext.setTenant("test");

    Credential credential1 = new Credential();
    credential1.setId(UUID.randomUUID());
    credential1.setUsername(EXISTED_USER);
    credential1.setPassword("pwd");

    credentialsRepository.save(credential1);

    Credential credential2 = new Credential();
    credential2.setId(UUID.randomUUID());
    credential2.setUsername(NON_EXISTED_USER);
    credential2.setPassword("pwd");

    credentialsRepository.save(credential2);
  }

  @Test
  void testCreateNonExistedUser() {
    securityManagerService.createBackgroundUser(NON_EXISTED_USER);
  }

  @Test
  void testCreateExistedUser() {
    securityManagerService.createBackgroundUser(EXISTED_USER);
  }

  @Test
  void testCreateNonPresentedUser() {
    assertThrows(IllegalStateException.class, () -> securityManagerService.createBackgroundUser(NON_PRESENTED_USER));
  }

  @Test
  void testLoginBackgroundUser() {
    String apiKey = securityManagerService.loginPubSubUser(EXISTED_USER);
    assertThat(apiKey, notNullValue());
  }

  @Test
  void testLoginBackgroundUserFailed() {
    assertThrows(IllegalStateException.class, () -> securityManagerService.loginPubSubUser(NON_PRESENTED_USER));
  }

}
