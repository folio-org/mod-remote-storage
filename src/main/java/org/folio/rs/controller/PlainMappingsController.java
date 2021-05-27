package org.folio.rs.controller;

import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.PlainMapping;
import org.folio.rs.domain.dto.PlainMappings;
import org.folio.rs.rest.resource.MappingsApi;
import org.folio.rs.service.LocationMappingsService;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;

@Log4j2
@Controller
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class PlainMappingsController implements MappingsApi {
  private static final String MAPPING_NOT_FOUND = "Mapping not found";

  private final LocationMappingsService locationMappingsService;

  @Override
  public ResponseEntity<PlainMapping> postMapping(@Valid PlainMapping mapping) {
    return new ResponseEntity<>(locationMappingsService.postPlainMapping(mapping), HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<PlainMapping> getMappingById(String id) {
    return ResponseEntity.ok().body(locationMappingsService.getPlainMapping(id));
  }

  @Override
  public ResponseEntity<PlainMappings> getMappings(@Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var mappings = locationMappingsService.getPlainMappings(offset, limit);
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
