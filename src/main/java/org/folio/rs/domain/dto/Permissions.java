package org.folio.rs.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

@Data
@AllArgsConstructor(staticName = "of")
public class Permissions {
  private String id;
  private String userId;
  private List<String> permissions;
}
