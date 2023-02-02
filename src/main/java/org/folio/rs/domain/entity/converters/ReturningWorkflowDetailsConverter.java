package org.folio.rs.domain.entity.converters;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.domain.dto.ReturningWorkflowDetails;

import jakarta.persistence.AttributeConverter;
import java.util.Objects;

public class ReturningWorkflowDetailsConverter implements AttributeConverter<ReturningWorkflowDetails, String> {

  @Override
  public String convertToDatabaseColumn(ReturningWorkflowDetails returningDetails) {
    if (Objects.isNull(returningDetails)) {
      return StringUtils.EMPTY;
    }
    return returningDetails.getValue();
  }

  @Override
  public ReturningWorkflowDetails convertToEntityAttribute(String value) {
    if (StringUtils.isEmpty(value)) {
      return null;
    }
    return ReturningWorkflowDetails.fromValue(value);
  }

}
