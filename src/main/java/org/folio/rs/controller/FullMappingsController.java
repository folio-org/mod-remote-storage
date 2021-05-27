package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.FullMapping;
import org.folio.rs.domain.dto.FullMappings;
import org.folio.rs.rest.resource.FullMappingsApi;
import org.folio.rs.service.LocationMappingsService;
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
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class FullMappingsController implements FullMappingsApi {
  private static final String MAPPING_NOT_FOUND = "Mapping not found";

  private final LocationMappingsService locationMappingsService;

  @Override
  public ResponseEntity<FullMapping> postFullMapping(@Valid FullMapping mapping) {
    return new ResponseEntity<>(locationMappingsService.postFullMapping(mapping), HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<FullMappings> getFullMappingsById(String id) {
    return ResponseEntity.ok().body(locationMappingsService.getFullMappings(id));
  }

  @Override
  public ResponseEntity<FullMappings> getFullMappings(@Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var mappings = locationMappingsService.getFullMappings(offset, limit);
    return new ResponseEntity<>(mappings, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> deleteFullMappingById(String mappingId) {
    locationMappingsService.deleteMappingById(mappingId);
    return ResponseEntity.noContent().build();
  }

  @Override
  public ResponseEntity<FullMappings> getFullMappingsLocations(@Min(0) @Max(2147483647) @Valid Integer offset,
      @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var mappings = locationMappingsService.getFullMappingsLocations();
    return new ResponseEntity<>(mappings, HttpStatus.OK);
  }

  @ExceptionHandler({EmptyResultDataAccessException.class, EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(MAPPING_NOT_FOUND);
  }
}
