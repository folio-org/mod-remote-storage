package org.folio.rs.client;

import org.folio.rs.domain.dto.PickupServicePoint;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "service-points")
public interface ServicePointsClient {

  @GetMapping(value = "/{servicePointId}", consumes = MediaType.APPLICATION_JSON_VALUE)
  PickupServicePoint getServicePoint(@PathVariable("servicePointId") String servicePointId);
}
