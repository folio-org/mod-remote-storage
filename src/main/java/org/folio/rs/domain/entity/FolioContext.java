package org.folio.rs.domain.entity;

import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@Component
public class FolioContext {
  private String okapiUrl;
  private String okapiToken;
  private String tenant;
}
