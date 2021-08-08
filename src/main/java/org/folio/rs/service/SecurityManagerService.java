package org.folio.rs.service;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.io.Resources;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.AuthnClient;
import org.folio.rs.client.PermissionsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.Permission;
import org.folio.rs.domain.dto.Permissions;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.SystemUserParameters;
import org.folio.rs.repository.SystemUserParametersRepository;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;


@Component
@Log4j2
@AllArgsConstructor
public class SecurityManagerService {

  private static final String PERMISSIONS_FILE_PATH = "permissions/system-user-permissions.csv";
  private static final String USER_LAST_NAME = "System";

  private final PermissionsClient permissionsClient;
  private final UsersClient usersClient;
  private final AuthnClient authnClient;
  private final SystemUserParametersRepository systemUserParametersRepository;
  @PersistenceContext
  private final EntityManager em;

  private final FolioModuleMetadata moduleMetadata;

  public void prepareOrUpdateSystemUser(String username, String password, String okapiUrl, String tenantId) {

    var systemUserParameters = buildDefaultSystemUserParameters(username, password, okapiUrl, tenantId);

    var folioUser = getFolioUser(username);

    if (folioUser.isPresent()) {
      updateUser(folioUser.get());
      addPermissions(folioUser.get().getId());
    } else {
      var userId = createFolioUser(username);
      saveCredentials(systemUserParameters);
      assignPermissions(userId);
    }

    var backgroundUserApiKey = loginSystemUser(systemUserParameters);
    systemUserParameters.setOkapiToken(backgroundUserApiKey);
    saveOrUpdateSystemUserParameters(systemUserParameters);
  }

  private String loginSystemUser(SystemUserParameters params) {
    var response = authnClient.getApiKey(params, params.getTenantId());
    var headers = response.getHeaders().get(XOkapiHeaders.TOKEN);
    if (CollectionUtils.isEmpty(headers)) {
      throw new IllegalStateException(String.format("User [%s] cannot log in", params.getUsername()));
    } else {
      return headers.get(0);
    }
  }

  private SystemUserParameters buildDefaultSystemUserParameters(String username, String password, String okapiUrl, String tenantId) {
    return SystemUserParameters.builder()
      .id(UUID.randomUUID())
      .username(username)
      .password(password)
      .okapiUrl(okapiUrl)
      .tenantId(tenantId).build();
  }

  /**
   * This method saves or replaces existing {@link SystemUserParameters} record if this record already exists for tenant
   * {@link SystemUserParameters#getTenantId()}
   *
   * @param systemUserParams - system user parameters to be saved or replaced (if it already exists for tenant)
   */
  private void saveOrUpdateSystemUserParameters(SystemUserParameters systemUserParams) {
    systemUserParametersRepository.getFirstByTenantId(systemUserParams.getTenantId())
      .ifPresent(existingSystemUserParameters -> systemUserParams.setId(existingSystemUserParameters.getId()));
    systemUserParametersRepository.save(systemUserParams);
  }

  @Cacheable(value = "systemUserParameters", key="#tenantId")
  public SystemUserParameters getSystemUserParameters(String tenantId) {
    final String sqlQuery = "SELECT * FROM " + moduleMetadata.getDBSchemaName(tenantId) + ".system_user_parameters";
    var query = em.createNativeQuery(sqlQuery, SystemUserParameters.class); //NOSONAR
    return (SystemUserParameters) query.getSingleResult();
  }

  private Optional<User> getFolioUser(String username) {
    var query = "username==" + username;
    var results = usersClient.getUsersByQuery(query);
    return results.getResult().stream().findFirst();
  }

  private String createFolioUser(String username) {
    final var user = createUserObject(username);
    final var id = user.getId();
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

  private void saveCredentials(SystemUserParameters systemUserParameters) {

    authnClient.saveCredentials(systemUserParameters);

    log.info("Saved credentials for user: [{}]", systemUserParameters.getUsername());
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
    var permissions = readPermissionsFromResource(PERMISSIONS_FILE_PATH);

    if (isEmpty(permissions)) {
      throw new IllegalStateException("No permissions found to assign to user with id: " + userId);
    }

    permissions.forEach(permission -> {
      var p = new Permission();
      p.setPermissionName(permission);
      try {
        permissionsClient.addPermission(userId, p);
      } catch (Exception e) {
        log.info("Error adding permission {} to System User. Permission may be already assigned.", permission);
      }
    });
  }

  private List<String> readPermissionsFromResource(String path) {
    List<String> permissions = new ArrayList<>();
    var url = Resources.getResource(path);

    try {
      permissions = Resources.readLines(url, StandardCharsets.UTF_8);
    } catch (IOException e) {
      log.error("Error reading permissions from {}", path);
    }

    return permissions;
  }

  private User createUserObject(String username) {
    final var user = new User();

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
