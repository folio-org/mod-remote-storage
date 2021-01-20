package org.folio.rs.service;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import org.folio.rs.client.AuthnClient;
import org.folio.rs.client.PermissionsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.Permission;
import org.folio.rs.domain.dto.Permissions;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.Credential;
import org.folio.rs.repository.CredentialsRepository;
import org.folio.spring.integration.XOkapiHeaders;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import com.google.common.io.Resources;

import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Component
@Log4j2
@AllArgsConstructor
public class SecurityManagerService {

  private static final String PERMISSIONS_FILE_PATH = "permissions/background-user-permissions.csv";
  private static final String USER_LAST_NAME = "System";

  private final PermissionsClient permissionsClient;
  private final UsersClient usersClient;
  private final AuthnClient authnClient;
  private final CredentialsRepository credentialsRepository;

  @Cacheable("api_keys")
  public String loginPubSubUser(String username) {
    List<Credential> credentials = credentialsRepository.findByUsername(username);
    if (credentials.isEmpty()) {
      throw new IllegalStateException("No credentials found to assign to user: " + username);
    } else {
      ResponseEntity<String> e = authnClient.getApiKey(credentials.get(0));
      return e.getHeaders()
        .get(XOkapiHeaders.TOKEN)
        .get(0);
    }
  }

  public void createBackgroundUser(String username) {
    var user = getExistingUser(username);

    if (Objects.nonNull(user)) {
      updateUser(user);
      addPermissions(user.getId());
    } else {
      var userId = createUser(username);
      saveCredentials(username);
      assignPermissions(userId);
    }
  }

  private User getExistingUser(String username) {
    String query = "username==" + username;
    ResultList<User> results = usersClient.query(query);
    if (results.getTotalRecords() != 0) {
      return results.getResult()
        .get(0);
    } else {
      return null;
    }
  }

  private String createUser(String username) {
    final User user = createUserObject(username);
    final String id = user.getId();
    usersClient.saveUser(user);
    return id;
  }

  private void updateUser(User existingUser) {
    log.info("Have to update  user [{}]", existingUser.getUsername());
    if (existingUserUpToDate(existingUser)) {
      log.info("The user [{}] is up to date", existingUser.getUsername());
    }
    usersClient.updateUser(existingUser.getId(), populateMissingUserProperties(existingUser));
    log.info("Update the user [{}]", existingUser.getId());
  }

  private void saveCredentials(String username) {
    List<Credential> credentials = credentialsRepository.findByUsername(username);

    if (isEmpty(credentials)) {
      throw new IllegalStateException("No credentials found to assign to user: " + username);
    }

    authnClient.saveCredentials(credentials.get(0));
    log.info("Saved credentials for user: [{}]", username);
  }

  private boolean assignPermissions(String userId) {
    List<String> perms = readPermissionsFromResource(PERMISSIONS_FILE_PATH);

    if (isEmpty(perms)) {
      throw new IllegalStateException("No permissions found to assign to user with id: " + userId);
    }

    var permissions = Permissions.of(UUID.randomUUID()
      .toString(), userId, perms);

    permissionsClient.assignPermissionsToUser(permissions);
    return true;
  }

  private void addPermissions(String userId) {
    List<String> permissions = readPermissionsFromResource(PERMISSIONS_FILE_PATH);

    if (isEmpty(permissions)) {
      throw new IllegalStateException("No permissions found to assign to user with id: " + userId);
    }

    permissions.forEach(permission -> {
      Permission p = new Permission();
      p.setPermissionName(permission);
      permissionsClient.addPermission(userId, p);
    });
  }

  private List<String> readPermissionsFromResource(String path) {
    List<String> permissions = new ArrayList<>();
    URL url = Resources.getResource(path);

    try {
      permissions = Resources.readLines(url, StandardCharsets.UTF_8);
    } catch (IOException e) {
      log.error("Error reading permissions from {}", path);
    }

    return permissions;
  }

  private User createUserObject(String username) {
    final User user = new User();

    user.setId(UUID.randomUUID()
      .toString());
    user.setActive(true);
    user.setUsername(username);

    user.setPersonal(new User.Personal());
    user.getPersonal()
      .setLastName(USER_LAST_NAME);

    return user;
  }

  private boolean existingUserUpToDate(User existingUser) {
    return existingUser.getPersonal() != null && isNotBlank(existingUser.getPersonal()
      .getLastName());
  }

  private User populateMissingUserProperties(User existingUser) {
    existingUser.setPersonal(new User.Personal());
    existingUser.getPersonal()
      .setLastName(USER_LAST_NAME);

    return existingUser;
  }
}
