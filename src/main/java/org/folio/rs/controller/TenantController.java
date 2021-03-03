package org.folio.rs.controller;

import static java.util.Objects.nonNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.validation.Valid;
import liquibase.exception.LiquibaseException;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.folio.rs.service.ConfigurationsService;
import org.folio.rs.service.LocationMappingsService;
import org.folio.rs.service.SecurityManagerService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.tenant.domain.dto.Parameter;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.folio.tenant.rest.resource.TenantApi;
import org.folio.util.pubsub.PubSubClientUtils;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@Log4j2
@RestController("folioTenantController")
@RequestMapping(value = "/_/")
@RequiredArgsConstructor
public class TenantController implements TenantApi {

  public static final String PARAMETER_LOAD_SAMPLE = "loadSample";
  private static final String SAMPLES_DIR = "samples";

  private final FolioSpringLiquibase folioSpringLiquibase;
  private final FolioExecutionContext context;
  private final ConfigurationsService configurationsService;
  private final LocationMappingsService locationMappingsService;
  private final SecurityManagerService securityManagerService;
  private final RetrievalQueueRepository retrievalQueueRepository;
  private final AccessionQueueRepository accessionQueueRepository;

  private final List<String> configurationSamples = Collections.singletonList("dematic.json");
  private final List<String> mappingSamples = Collections.singletonList("annex_to_dematic.json");
  private final List<String> retrievalQueueSamples = Collections.singletonList("retrieval_queue_record.json");
  private final List<String> accessionQueueSamples = Collections.singletonList("accession_queue_record.json");

  public static final String SYSTEM_USER = "system-user";


  @SneakyThrows
  @Override
  public ResponseEntity<String> postTenant(@Valid TenantAttributes tenantAttributes) {
    var tenantId = context.getTenantId();

    if (folioSpringLiquibase != null) {

      var schemaName = context.getFolioModuleMetadata()
        .getDBSchemaName(tenantId);

      folioSpringLiquibase.setDefaultSchema(schemaName);
      try {
        folioSpringLiquibase.performLiquibaseUpdate();

        if (isLoadSample(tenantAttributes)) {
          loadSampleData();
        }
      } catch (LiquibaseException e) {
        e.printStackTrace();
        log.error("Liquibase error", e);
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Liquibase error: " + e.getMessage());
      }
    }
    registerModuleToPubSub();
    initializeSystemUser(tenantId);
    return ResponseEntity.ok().body("true");
  }

  private void initializeSystemUser(String tenantId) {
    try {
      securityManagerService.prepareSystemUser(SYSTEM_USER, SYSTEM_USER, context.getOkapiUrl(), tenantId);
    } catch (Exception e) {
      log.error("Error initializing System User", e);
    }
  }

  private void registerModuleToPubSub() {
    PubSubClientUtils.registerModule(new OkapiConnectionParams(getHeadersMap(), null));
  }

  private Map<String, String> getHeadersMap() {
    Map<String, String> headers = new HashMap<>();
    headers.put(XOkapiHeaders.URL, context.getOkapiUrl());
    headers.put(XOkapiHeaders.TENANT, context.getTenantId());
    headers.put(XOkapiHeaders.TOKEN, context.getToken());
    return headers;
  }

  private void loadSampleData() {
    log.info("Loading sample data");
    readEntitiesFromFiles(configurationSamples, StorageConfiguration.class)
      .forEach(configurationsService::postConfiguration);
    readEntitiesFromFiles(mappingSamples, LocationMapping.class)
      .forEach(locationMappingsService::postMapping);
    readEntitiesFromFiles(retrievalQueueSamples, RetrievalQueueRecord.class)
        .forEach(retrievalQueueRepository::save);
    readEntitiesFromFiles(accessionQueueSamples, AccessionQueueRecord.class)
        .forEach(accessionQueueRepository::save);
  }

  private <T> List<T> readEntitiesFromFiles(List<String> filenames, Class<T> type) {
    return filenames.stream()
      .map(fileName -> {
        try {
          return new ObjectMapper()
              .registerModule(new JavaTimeModule())
              .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
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
