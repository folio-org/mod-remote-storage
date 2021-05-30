package org.folio.rs.controller;

import static java.util.function.UnaryOperator.identity;
import static org.folio.rs.TestUtils.ITEM_BARCODE;
import static org.folio.rs.TestUtils.MAPPER;
import static org.folio.rs.TestUtils.buildBaseEventPayload;
import static org.folio.rs.TestUtils.buildCheckInEventPayload;
import static org.folio.rs.TestUtils.buildRequestChangedEventPayload;
import static org.folio.rs.TestUtils.buildRequestCreatedEventPayload;
import static org.folio.rs.domain.dto.Request.RequestType.HOLD;
import static org.folio.rs.domain.dto.Request.RequestType.PAGE;
import static org.folio.rs.domain.dto.ReturningWorkflowDetails.FOLIO;
import static org.folio.rs.domain.entity.ProviderRecord.CAIA_SOFT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.PubSubEvent;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.TimeUnits;
import org.folio.rs.domain.entity.LocationMapping;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.repository.LocationMappingsRepository;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.rs.service.ConfigurationsService;
import org.folio.rs.util.LogEventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.JsonProcessingException;

import lombok.extern.log4j.Log4j2;

@ExtendWith(MockitoExtension.class)
@Log4j2
public class PubSubEventControllerTest extends TestBase {

  private static final String PUB_SUB_HANDLER_URL = "http://localhost:%s/remote-storage/pub-sub-handlers/log-record-event";

  @Autowired
  private ReturnRetrievalQueueRepository returnRetrievalQueueRepository;

  @Autowired
  private LocationMappingsRepository locationMappingsRepository;

  @Autowired
  private ConfigurationsService configurationsService;

  @BeforeEach
  void prepare() {
    returnRetrievalQueueRepository.deleteAll();
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_MOVED", "REQUEST_UPDATED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldProcessChangedEvent(LogEventType logEventType) throws JsonProcessingException {
    log.info("=== Should process created event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(logEventType.value());
    pubSubEvent.setPayload(buildRequestChangedEventPayload(HOLD.value(), PAGE.value()));

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_MOVED", "REQUEST_UPDATED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessChangedEvent(LogEventType logEventType) throws JsonProcessingException {
    log.info("=== Should not process changed event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(logEventType.value());
    pubSubEvent.setPayload(buildRequestChangedEventPayload(PAGE.value(), PAGE.value()));

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), nullValue());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_CREATED",
      "REQUEST_CREATED_THROUGH_OVERRIDE" }, mode = EnumSource.Mode.INCLUDE)
  void shouldProcessCreatedEvent(LogEventType logEventType) throws JsonProcessingException {
    log.info("=== Should process changed event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(logEventType.value());
    pubSubEvent.setPayload(buildRequestCreatedEventPayload(PAGE.value()));

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "REQUEST_CREATED",
      "REQUEST_CREATED_THROUGH_OVERRIDE" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessCreatedEvent(LogEventType logEventType) throws JsonProcessingException {
    log.info("=== Should not process created event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(logEventType.value());
    pubSubEvent.setPayload(buildRequestCreatedEventPayload(HOLD.value()));

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), nullValue());
  }

  @ParameterizedTest
  @EnumSource(value = LogEventType.class, names = { "LOAN", "NOTICE", "CHECK_IN", "CHECK_OUT",
      "REQUEST_REORDERED" }, mode = EnumSource.Mode.INCLUDE)
  void shouldNotProcessOtherEventTypes(LogEventType logEventType) throws JsonProcessingException {
    log.info("=== Should not process event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(logEventType.value());
    pubSubEvent.setPayload(buildBaseEventPayload());

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), nullValue());
  }

  @Test
  void shouldProcessItemCheckInEvent() throws JsonProcessingException {
    log.info("=== Should process item check in event ===");
    var pubSubEvent = new PubSubEvent();
    pubSubEvent.setLogEventType(LogEventType.CHECK_IN.value());
    pubSubEvent.setPayload(buildCheckInEventPayload());


    var configuration = new StorageConfiguration().id("b3354743-285d-468d-9fa1-4e3d6321c13d")
      .name("Remote Storage")
      .apiKey("i+X9dfNOztkBfoAmGTXf/w==")
      .providerName(CAIA_SOFT.getId())
      .returningWorkflowDetails(FOLIO)
      .url("https://rs.dematic.com")
      .accessionDelay(2)
      .accessionTimeUnit(TimeUnits.MINUTES);

    configurationsService.postConfiguration(configuration);

    LocationMapping locationMapping = new LocationMapping();
    locationMapping.setConfigurationId(UUID.fromString(configuration.getId()));
    locationMapping.setFolioLocationId(UUID.fromString("53cf956f-c1df-410b-8bea-27f712cca7c0"));

    locationMappingsRepository.save(locationMapping);

    post(String.format(PUB_SUB_HANDLER_URL, okapiPort), MAPPER.writeValueAsString(pubSubEvent), String.class);
    Map<String, ReturnRetrievalQueueRecord> records = returnRetrievalQueueRepository.findAll().stream().collect(Collectors.toMap(ReturnRetrievalQueueRecord::getItemBarcode, identity()));
    assertThat(records.get(ITEM_BARCODE), notNullValue());
  }
}
