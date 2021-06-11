package org.folio.rs.controller;

import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
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
public class MappingsController implements MappingsApi {
  private static final String MAPPING_NOT_FOUND = "Mapping not found";

  private final LocationMappingsService locationMappingsService;

  @Override
  public ResponseEntity<RemoteLocationConfigurationMapping> postMapping(@Valid RemoteLocationConfigurationMapping mapping) {
    return new ResponseEntity<>(locationMappingsService.postRemoteLocationConfigurationMapping(mapping), HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<RemoteLocationConfigurationMapping> getMappingById(String id) {
    return ResponseEntity.ok().body(locationMappingsService.getRemoteLocationConfigurationMapping(id));
  }

  @Override
  public ResponseEntity<RemoteLocationConfigurationMappings> getMappings(@Valid String finalLocationId,
    @Valid  String remoteConfigurationId, @Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit) {
    var mappings = locationMappingsService.getRemoteLocationConfigurationMappings(buildFilterData(finalLocationId, remoteConfigurationId, offset, limit));
    return new ResponseEntity<>(mappings, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> deleteMappingById(String finalLocationId) {
    locationMappingsService.deleteMappingById(finalLocationId);
    return ResponseEntity.noContent().build();
  }

  @ExceptionHandler({EmptyResultDataAccessException.class, EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(MAPPING_NOT_FOUND);
  }

  private LocationMappingFilterData buildFilterData(String finalLocationId, String remoteConfigurationId,
    Integer offset, Integer limit) {
    return LocationMappingFilterData
      .builder()
      .finalLocationId(finalLocationId)
      .remoteConfigurationId(remoteConfigurationId)
      .offset(offset)
      .limit(limit)
      .build();
  }
}
