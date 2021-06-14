package org.folio.rs.domain.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(toBuilder = true)
public class AccessionFilterData {

  private final Boolean isPresented;
  private final String remoteStorageConfigurationId;
  private final String createDate;
  private final int offset;
  private final int limit;
}
