package org.folio.rs.domain;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import org.folio.rs.domain.entity.SystemUserParameters;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;

@Getter
@Builder
@AllArgsConstructor
public class AsyncFolioExecutionContext implements FolioExecutionContext {

  private final UUID userId;

  private final String userName;

  private final String password;

  private final String token;

  private final String okapiUrl;

  private final String tenantId;

  private final FolioModuleMetadata moduleMetadata;

  public AsyncFolioExecutionContext(SystemUserParameters parameters,
    FolioModuleMetadata moduleMetadata) {
    this.userId = parameters.getId();
    this.userName = parameters.getUsername();
    this.okapiUrl = parameters.getOkapiUrl();
    this.password = parameters.getPassword();
    this.token = parameters.getOkapiToken();
    this.tenantId = parameters.getTenantId();
    this.moduleMetadata = moduleMetadata;
  }

  @Override
  public Map<String, Collection<String>> getAllHeaders() {
    throw new UnsupportedOperationException("getAllHeaders");
  }

  @Override
  public Map<String, Collection<String>> getOkapiHeaders() {
    throw new UnsupportedOperationException("getOkapiHeaders");
  }

  @Override
  public FolioModuleMetadata getFolioModuleMetadata() {
    return moduleMetadata;
  }
}
