package org.folio.rs.controller;

import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.rest.resource.MappingsApi;
import org.folio.rs.service.LocationMappingsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;

@Log4j2
@Controller
@RequestMapping(value = "/remote-storage/")
public class LocationMappingsController implements MappingsApi {
  private static final String MAPPING_NOT_FOUND = "Mapping not found";

  private final LocationMappingsService locationMappingsService;

  @Autowired
  public LocationMappingsController(LocationMappingsService locationMappingsService) {
    this.locationMappingsService = locationMappingsService;
  }

  @Override
  public ResponseEntity<LocationMapping> postMapping(@Valid LocationMapping locationMapping) {
    return ResponseEntity.ok().body(locationMappingsService.postMapping(locationMapping));
  }

  @Override
  public ResponseEntity<String> deleteMappingById(String mappingId) {
    locationMappingsService.deleteMappingById(mappingId);
    return ResponseEntity.noContent().build();
  }

  @ExceptionHandler({EmptyResultDataAccessException.class, EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(MAPPING_NOT_FOUND);
  }
}
