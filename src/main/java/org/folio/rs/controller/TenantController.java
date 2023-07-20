package org.folio.rs.controller;

import static java.util.Objects.nonNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import jakarta.validation.Valid;
import liquibase.exception.LiquibaseException;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.rs.service.ConfigurationsService;
import org.folio.rs.service.KafkaService;
import org.folio.rs.service.LocationMappingsService;
import org.folio.rs.service.SecurityManagerService;
import org.folio.rs.service.PubSubService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.spring.service.TenantService;
import org.folio.tenant.domain.dto.Parameter;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.folio.tenant.rest.resource.TenantApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@Log4j2
@RestController("folioTenantController")
@RequestMapping
@RequiredArgsConstructor
public class TenantController implements TenantApi {

  public static final String PARAMETER_LOAD_SAMPLE = "loadSample";
  private static final String SAMPLES_DIR = "samples";

  private final FolioSpringLiquibase folioSpringLiquibase;
  private final FolioExecutionContext context;
  private final ConfigurationsService configurationsService;
  private final LocationMappingsService locationMappingsService;
  private final SecurityManagerService securityManagerService;
  private final ReturnRetrievalQueueRepository returnRetrievalQueueRepository;
  private final AccessionQueueRepository accessionQueueRepository;
  private final PubSubService pubSubService;
  private final KafkaService kafkaService;
  private final TenantService tenantService;

  private final List<String> configurationSamples = List.of("dematic_configuration_sample.json", "caia_soft_configuration_sample.json");
  private final List<String> mappingSamples = Collections.singletonList("annex_to_dematic_full.json");
  private final List<String> retrievalQueueSamples = List.of("retrieval_queue_record.json", "retrieval_queue_record_for_caia_soft.json");
  private final List<String> accessionQueueSamples = Collections.singletonList("accession_queue_record.json");

  @Value("${remote-storage.system-user.username}")
  private String username;
  @Value("${remote-storage.system-user.password}")
  private String password;



  @SneakyThrows
  @Override
  public ResponseEntity<Void> postTenant(@Valid TenantAttributes tenantAttributes) {
    var tenantId = context.getTenantId();

    kafkaService.restartEventListeners();

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
        log.error("Liquibase error", e);
        return ResponseEntity.internalServerError().build();
      }
    }

    try {
      pubSubService.registerPubSubModule(context.getOkapiUrl(), tenantId, context.getToken());
    } catch(Exception e) {
      log.error("Error during pub-sub registration:", e);
    }

    try {
      initializeSystemUser(tenantId);
    } catch(Exception e) {
      log.error("Error during system-user initialization:", e);
    }

    return ResponseEntity.noContent().build();
  }

  @Override
  public ResponseEntity<Void> deleteTenant(String operationId) {
    pubSubService.unregisterPubSubModule(context.getOkapiUrl(), context.getTenantId(), context.getToken());
    tenantService.deleteTenant(null);
    return ResponseEntity.noContent().build();
  }

  private void initializeSystemUser(String tenantId) {
    try {
      securityManagerService.prepareOrUpdateSystemUser(username, password, context.getOkapiUrl(), tenantId);
    } catch (Exception e) {
      log.error("Error initializing System User", e);
    }
  }


  private void loadSampleData() {
    log.info("Loading sample data");
    readEntitiesFromFiles(configurationSamples, StorageConfiguration.class)
      .forEach(configurationsService::postConfiguration);
    readEntitiesFromFiles(mappingSamples, ExtendedRemoteLocationConfigurationMapping.class)
      .forEach(locationMappingsService::postExtendedRemoteLocationConfigurationMapping);
    readEntitiesFromFiles(retrievalQueueSamples, ReturnRetrievalQueueRecord.class)
        .forEach(returnRetrievalQueueRepository::save);
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
              .readValue(this.getClass().getClassLoader().getResourceAsStream(SAMPLES_DIR + "/" + fileName), type);
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
