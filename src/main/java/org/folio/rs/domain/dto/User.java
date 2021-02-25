package org.folio.rs.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class User {
  private String id;
  private String username;
  private String barcode;
  private boolean active;
  private Personal personal;

  @Data
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Personal {
    private String lastName;
  }
}
