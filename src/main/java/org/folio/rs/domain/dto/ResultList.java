package org.folio.rs.domain.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAlias;

import lombok.Data;

@Data
public class ResultList<E> {
  @JsonAlias("total_records")
  private Integer totalRecords;
  @JsonAlias({ "instances", "users", "items", "requests", "holdingsRecords", "identifierTypes", "contributorTypes", "locations"})
  private List<E> result;

  public boolean isEmpty() {
    return result == null || result.isEmpty();
  }
}
