package org.folio.rs.controller;

import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
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
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

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
  public ResponseEntity<LocationMapping> getMappingById(String id) {
    return ResponseEntity.ok().body(locationMappingsService.getMappingByFolioLocationId(id));
  }

  @Override
  public ResponseEntity<LocationMappings> getMappings(@Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var mappings = locationMappingsService.getMappings(offset, limit);
    return new ResponseEntity<>(mappings, HttpStatus.OK);
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
