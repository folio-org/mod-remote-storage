package org.folio.rs.domain.dto;

import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(toBuilder = true)
public class FilterData {

  private final Boolean accessioned;
  private final String storageId;
  private final LocalDateTime createDate;
  private final int offset;
  private final int limit;
}
