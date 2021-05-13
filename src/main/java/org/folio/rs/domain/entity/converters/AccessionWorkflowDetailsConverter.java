package org.folio.rs.domain.entity.converters;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.domain.dto.AccessionWorkflowDetails;

import javax.persistence.AttributeConverter;
import java.util.Objects;

public class AccessionWorkflowDetailsConverter
  implements AttributeConverter<AccessionWorkflowDetails, String> {

  @Override
  public String convertToDatabaseColumn(AccessionWorkflowDetails accessionDetails) {
    if (Objects.isNull(accessionDetails)) {
      return StringUtils.EMPTY;
    }
    return accessionDetails.getValue();
  }

  @Override
  public AccessionWorkflowDetails convertToEntityAttribute(String value) {
    if (StringUtils.isEmpty(value)) {
      return null;
    }
    return AccessionWorkflowDetails.fromValue(value);
  }
}
