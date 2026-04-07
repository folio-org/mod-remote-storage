package org.folio.rs.client;

import org.folio.rs.domain.dto.PickupServicePoint;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange(value = "service-points")
public interface ServicePointsClient {

  @GetExchange("/{servicePointId}")
  PickupServicePoint getServicePoint(@PathVariable String servicePointId);
}
