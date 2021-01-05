package org.folio.rs.domain.dto;//package org.folio.rs.domain.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonAlias;

import lombok.Data;

@Data
public class ResultList<E> {
  @JsonAlias("total_records")
  private Integer totalRecords;
  @JsonAlias({ "instances"})
  private List<E> result;
}
