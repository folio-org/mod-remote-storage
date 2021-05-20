package org.folio.rs.client;

import org.folio.rs.domain.dto.FolioLocation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient("locations")
public interface LocationClient {
    @GetMapping(value = "/{folioLocationId}", consumes = MediaType.APPLICATION_JSON_VALUE)
    FolioLocation getLocation(@PathVariable("folioLocationId") String folioLocationId);
}
