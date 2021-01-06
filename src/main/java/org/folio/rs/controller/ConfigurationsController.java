package org.folio.rs.controller;

import static java.util.Objects.isNull;

import java.util.List;
import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.Provider;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.StorageConfigurations;
import org.folio.rs.rest.resource.ConfigurationsApi;
import org.folio.rs.service.ConfigurationsService;
import org.folio.rs.service.ProviderService;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@Log4j2
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage/")
public class ConfigurationsController implements ConfigurationsApi {
  private static final String CONFIGURATION_NOT_FOUND = "Configuration not found";

  private final ConfigurationsService configurationsService;
  private final ProviderService providerService;

  @Override
  public ResponseEntity<String> deleteConfigurationById(String configId) {
    configurationsService.deleteConfigurationById(configId);
    return ResponseEntity.noContent().build();
  }

  @Override
  public ResponseEntity<StorageConfiguration> getConfigurationById(String configId) {
    var configuration = configurationsService.getConfigurationById(configId);
    return isNull(configuration) ? ResponseEntity.notFound().build() : new ResponseEntity<>(configuration, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<StorageConfigurations> getConfigurations(@Min(0) @Max(2147483647) @Valid Integer offset,
    @Min(0) @Max(2147483647) @Valid Integer limit, @Valid String query) {
    var configurations = configurationsService.getConfigurations(offset, limit);
    return new ResponseEntity<>(configurations, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<StorageConfiguration> postConfiguration(@Valid StorageConfiguration storageConfiguration) {
    var configuration = configurationsService.postConfiguration(storageConfiguration);
    return new ResponseEntity<>(configuration, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> putConfiguration(@Pattern(regexp = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$") String configId,
    @Valid StorageConfiguration storageConfiguration) {
    configurationsService.updateConfiguration(configId, storageConfiguration);
    return ResponseEntity.noContent().build();
  }

  @ExceptionHandler({EmptyResultDataAccessException.class, EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(CONFIGURATION_NOT_FOUND);
  }

  @RequestMapping("/providers")
  @ResponseBody
  public ResponseEntity<List<Provider>> getProviders() {
    return new ResponseEntity<>(providerService.getProviders(), HttpStatus.OK);
  }
}
