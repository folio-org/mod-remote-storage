package org.folio.rs.domain.entity;

import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;

@Component
@Setter
@Getter
public class GlobalValue {
  private String okapiUrl;
}
