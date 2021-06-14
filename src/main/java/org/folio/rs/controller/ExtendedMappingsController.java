package org.folio.rs.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.rest.resource.ExtendedMappingsApi;
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
public class ExtendedMappingsController implements ExtendedMappingsApi {
  private static final String MAPPING_NOT_FOUND = "Mapping not found";

  private final LocationMappingsService locationMappingsService;

  @Override
  public ResponseEntity<ExtendedRemoteLocationConfigurationMapping> postExtendedRemoteLocationConfigurationMapping(@Valid ExtendedRemoteLocationConfigurationMapping mapping) {
    return new ResponseEntity<>(locationMappingsService.postExtendedRemoteLocationConfigurationMapping(mapping), HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<ExtendedRemoteLocationConfigurationMappings> getExtendedRemoteLocationConfigurationMappingsById(String id) {
    return ResponseEntity.ok().body(locationMappingsService.getExtendedRemoteLocationConfigurationMappings(id));
  }

  @Override
  public ResponseEntity<ExtendedRemoteLocationConfigurationMappings> getExtendedRemoteLocationConfigurationMappings(@Valid String finalLocationId,
    @Valid String remoteStorageConfigurationId, @Valid String originalLocationId, @Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit) {
    var mappings = locationMappingsService.getExtendedRemoteLocationConfigurationMappings(buildFilterData(finalLocationId, remoteStorageConfigurationId, originalLocationId, offset, limit));
    return new ResponseEntity<>(mappings, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> deleteOriginalLocationByRemoteStorageConfigurationIdAndOriginalLocationId(String remoteStorageConfigurationId, String originalLocationId) {
    locationMappingsService.removeOriginalLocationIdFromExistingEntities(remoteStorageConfigurationId, originalLocationId);
    return ResponseEntity.noContent().build();
  }

  @Override
  public ResponseEntity<ExtendedRemoteLocationConfigurationMappings> getExtendedRemoteLocationConfigurationMappingsLocations(@Valid String finalLocationId,
    @Valid String remoteStorageConfigurationId, @Valid String originalLocationId, @Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit) {
    var mappings = locationMappingsService.getExtendedRemoteLocationConfigurationMappingsLocations(buildFilterData(finalLocationId, remoteStorageConfigurationId, originalLocationId, offset, limit));
    return new ResponseEntity<>(mappings, HttpStatus.OK);
  }

  @ExceptionHandler({EmptyResultDataAccessException.class, EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(MAPPING_NOT_FOUND);
  }

  private LocationMappingFilterData buildFilterData(String finalLocationId, String remoteStorageConfigurationId, String originalLocationId,
    Integer offset, Integer limit) {
    return LocationMappingFilterData
      .builder()
      .finalLocationId(finalLocationId)
      .remoteStorageConfigurationId(remoteStorageConfigurationId)
      .originalLocationId(originalLocationId)
      .offset(offset)
      .limit(limit)
      .build();
  }
}
