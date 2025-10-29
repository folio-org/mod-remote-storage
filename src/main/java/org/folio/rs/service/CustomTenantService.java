package org.folio.rs.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.spring.service.PrepareSystemUserService;
import org.folio.spring.service.TenantService;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

@Lazy
@Log4j2
@Primary
@Service
public class CustomTenantService extends TenantService {

  private static final String SAMPLES_DIR = "samples";

  private final KafkaService kafkaService;
  private final PubSubService pubSubService;
  private final ConfigurationsService configurationsService;
  private final LocationMappingsService locationMappingsService;
  private final PrepareSystemUserService prepareSystemUserService;
  private final AccessionQueueRepository accessionQueueRepository;
  private final ReturnRetrievalQueueRepository returnRetrievalQueueRepository;

  private final List<String> configurationSamples = List.of("dematic_configuration_sample.json", "caia_soft_configuration_sample.json");
  private final List<String> mappingSamples = List.of("annex_to_dematic_full.json");
  private final List<String> retrievalQueueSamples = List.of("retrieval_queue_record.json", "retrieval_queue_record_for_caia_soft.json");
  private final List<String> accessionQueueSamples = List.of("accession_queue_record.json");

  public CustomTenantService(
    JdbcTemplate jdbcTemplate,
    FolioExecutionContext context,
    FolioSpringLiquibase folioSpringLiquibase,
    KafkaService kafkaService,
    PubSubService pubSubService,
    ConfigurationsService configurationsService,
    LocationMappingsService locationMappingsService,
    PrepareSystemUserService prepareSystemUserService,
    ReturnRetrievalQueueRepository returnRetrievalQueueRepository,
    AccessionQueueRepository accessionQueueRepository) {
    super(jdbcTemplate, context, folioSpringLiquibase);
    this.kafkaService = kafkaService;
    this.pubSubService = pubSubService;
    this.configurationsService = configurationsService;
    this.locationMappingsService = locationMappingsService;
    this.prepareSystemUserService = prepareSystemUserService;
    this.returnRetrievalQueueRepository = returnRetrievalQueueRepository;
    this.accessionQueueRepository = accessionQueueRepository;
  }

  @Override
  protected void beforeTenantUpdate(TenantAttributes tenantAttributes) {
    kafkaService.restartEventListeners();
  }

  @Override
  protected void afterTenantUpdate(TenantAttributes tenantAttributes) {
    try {
      pubSubService.registerPubSubModule(
        context.getOkapiUrl(), context.getTenantId(), context.getToken());
    } catch(Exception e) {
      log.error("Error during pub-sub registration:", e);
    }

    try {
      prepareSystemUserService.setupSystemUser();
    } catch(Exception e) {
      log.error("Error during system-user initialization:", e);
    }
  }

  @Override
  protected void afterTenantDeletion(TenantAttributes tenantAttributes) {
    pubSubService.unregisterPubSubModule(
      context.getOkapiUrl(), context.getTenantId(), context.getToken());
  }

  @Override
  public void loadSampleData() {
    super.loadSampleData();
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
      .toList();
  }
}
