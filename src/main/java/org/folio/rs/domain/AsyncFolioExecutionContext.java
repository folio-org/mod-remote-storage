package org.folio.rs.domain;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
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
