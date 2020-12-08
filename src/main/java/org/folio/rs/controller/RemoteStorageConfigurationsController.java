package org.folio.rs.controller;

import static java.util.Objects.isNull;
import static org.folio.rs.util.ErrorUtil.CONFIGURATION_NOT_FOUND;
import static org.folio.rs.util.ErrorUtil.buildValidationError;

import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.Errors;
import org.folio.rs.domain.dto.RemoteStorageConfig;
import org.folio.rs.domain.dto.RemoteStorageConfigCollection;
import org.folio.rs.rest.resource.ConfigurationsApi;
import org.folio.rs.service.RemoteStorageConfigurationsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

@Log4j2
@RestController
@RequestMapping(value = "/remote-storages/")
public class RemoteStorageConfigurationsController implements ConfigurationsApi {

  private final RemoteStorageConfigurationsService configurationsService;

  @Autowired
  public RemoteStorageConfigurationsController(RemoteStorageConfigurationsService configurationsService) {
    this.configurationsService = configurationsService;
  }

  @Override
  public ResponseEntity<String> deleteConfigurationById(String configId) {
    configurationsService.deleteConfigurationById(configId);
    return ResponseEntity.noContent().build();
  }

  @Override
  public ResponseEntity<RemoteStorageConfig> getConfigurationById(String configId) {
    var configuration = configurationsService.getConfigurationById(configId);
    return isNull(configuration) ? ResponseEntity.notFound().build() : new ResponseEntity<>(configuration, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<RemoteStorageConfigCollection> getConfigurations(@Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var configurations = configurationsService.getConfigurations(offset, limit, "");
    return new ResponseEntity<>(configurations, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<RemoteStorageConfig> postConfiguration(@Valid RemoteStorageConfig remoteConfig) {
    var configuration = configurationsService.postConfiguration(remoteConfig);
    return new ResponseEntity<>(configuration, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<RemoteStorageConfig> putConfiguration(@Valid RemoteStorageConfig remoteConfig) {
    var configuration = configurationsService.createOrUpdateConfiguration(remoteConfig);
    return new ResponseEntity<>(configuration, HttpStatus.OK);
  }

  @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public Errors handleValidationExceptions(MethodArgumentNotValidException ex) {
    return buildValidationError(ex);
  }

  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ExceptionHandler({ EmptyResultDataAccessException.class, EntityNotFoundException.class })
  public String handleNotFoundExceptions() {
    return CONFIGURATION_NOT_FOUND;
  }
}
