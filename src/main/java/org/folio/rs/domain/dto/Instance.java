package org.folio.rs.domain.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Getter;

@JsonIgnoreProperties(ignoreUnknown = true)
@Getter
public class Instance {
  private String title;
  private List<Contributor> contributors = new ArrayList<>();
}
