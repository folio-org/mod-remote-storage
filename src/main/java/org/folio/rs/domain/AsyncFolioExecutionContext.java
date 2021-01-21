package org.folio.rs.domain;

import java.util.Collection;
import java.util.Map;
import lombok.Getter;
import org.folio.rs.domain.entity.SystemUserParameters;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;

@Getter
public class AsyncFolioExecutionContext implements FolioExecutionContext {

  private final String userName;

  private final String password;

  private final String token;

  private final String okapiUrl;

  private final String tenantId;

  public AsyncFolioExecutionContext(SystemUserParameters parameters) {
    this.userName = parameters.getId();
    this.okapiUrl = parameters.getOkapiUrl();
    this.password = parameters.getPassword();
    this.token = parameters.getOkapiToken();
    this.tenantId = parameters.getTenantId();
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
    throw new UnsupportedOperationException("getFolioModuleMetadata");
  }
}
