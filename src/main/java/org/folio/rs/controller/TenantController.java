package org.folio.rs.controller;

import static java.util.Objects.nonNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import liquibase.exception.LiquibaseException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.service.ConfigurationsService;
import org.folio.rs.service.LocationMappingsService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.tenant.domain.dto.Parameter;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.folio.tenant.rest.resource.TenantApi;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Log4j2
@RestController("folioTenantController")
@RequiredArgsConstructor
@RequestMapping(value = "/_/")
public class TenantController implements TenantApi {
  private static final String PARAMETER_LOAD_SAMPLE = "loadSample";
  private static final String SAMPLES_DIR = "samples";

  private final FolioSpringLiquibase folioSpringLiquibase;
  private final FolioExecutionContext context;
  private final ConfigurationsService configurationsService;
  private final LocationMappingsService locationMappingsService;
  private final List<String> configurationSamples = Collections.singletonList("dematic.json");
  private final List<String> mappingSamples = Collections.singletonList("annex_to_dematic.json");

  @Override
  public ResponseEntity<String> postTenant(@Valid TenantAttributes tenantAttributes) {
    if (folioSpringLiquibase != null) {
      var tenantId = context.getTenantId();

      var schemaName = context.getFolioModuleMetadata().getDBSchemaName(tenantId);

      folioSpringLiquibase.setDefaultSchema(schemaName);
      try {
        folioSpringLiquibase.performLiquibaseUpdate();

        if (isLoadSample(tenantAttributes)) {
          loadSampleData();
        }
      } catch (LiquibaseException e) {
        e.printStackTrace();
        log.error("Liquibase error", e);
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Liquibase error: " + e.getMessage());
      }
    }
    return ResponseEntity.ok().body("true");
  }

  private void loadSampleData() {
    log.info("Loading sample data");
    readEntitiesFromFiles(configurationSamples, StorageConfiguration.class).forEach(configurationsService::postConfiguration);
    readEntitiesFromFiles(mappingSamples, LocationMapping.class).forEach(locationMappingsService::postMapping);
  }

  private <T> List<T> readEntitiesFromFiles(List<String> filenames, Class<T> type) {
    return filenames.stream()
      .map(fileName -> {
        try {
          return new ObjectMapper()
            .readValue(new ClassPathResource(SAMPLES_DIR + "/" + fileName).getFile(), type);
        } catch (IOException e) {
          log.error("Error loading " + fileName, e);
          return null;
        }
      })
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
  }

  private boolean isLoadSample(TenantAttributes tenantAttributes) {
    if (nonNull(tenantAttributes.getParameters())) {
      for (Parameter parameter : tenantAttributes.getParameters()) {
        if (PARAMETER_LOAD_SAMPLE.equals(parameter.getKey())) {
          return Boolean.parseBoolean(parameter.getValue());
        }
      }
    }
    return false;
  }
}
