package org.folio.rs.integration;

import static org.awaitility.Awaitility.await;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hibernate.validator.internal.util.Contracts.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import io.netty.util.internal.StringUtil;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.serialization.StringSerializer;
import org.awaitility.Duration;
import org.folio.rs.controller.ControllerTestBase;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.DomainEvent;
import org.folio.rs.domain.entity.DomainEventType;
import org.folio.rs.domain.entity.GlobalValue;
import org.folio.rs.domain.entity.MappingRecord;
import org.folio.rs.dto.EffectiveCallNumberComponents;
import org.folio.rs.dto.Item;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.repository.LocationMappingsRepository;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.test.EmbeddedKafkaBroker;
import org.springframework.kafka.test.context.EmbeddedKafka;
import org.springframework.kafka.test.utils.KafkaTestUtils;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.util.SocketUtils;
import org.springframework.web.client.HttpClientErrorException;

@EmbeddedKafka(topics = { "inventory.items" }, ports = { 9092 })
@TestPropertySource("classpath:application-test.properties")
@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class KafkaIntegrationTest extends ControllerTestBase {

  private static final String ACCESSION_URL = "http://localhost:%s/remote-storage/accession";
  @Autowired
  private KafkaMessageListener messageListener;
  @Autowired
  private EmbeddedKafkaBroker embeddedKafkaBroker;
  @Autowired
  private AccessionQueueRepository accessionQueueRepository;
  @Autowired
  private LocationMappingsRepository locationMappingsRepository;
  @Autowired
  private GlobalValue globalValue;

  private static final String TOPIC = "inventory.items";
  private static final String CALL_NUMBER = "+1-111-22-33";
  private static final String BARCODE = "1234567890";
  private static final String INSTANCE_ID = "a89eccf0-57a6-495e-898d-32b9b2210f2f";
  private static final String OLD_EFFECTIVE_LOCATION_ID = "59a54d96-2563-4f55-a6b0-fb23cb5760af";
  private static final String NEW_EFFECTIVE_LOCATION_ID = "cefb6261-a8d7-44f1-8264-90667d6c45d3";
  private static final String REMOTE_STORAGE_ID = "0f976099-2c7a-48ca-9271-3523905eab6b";
  private static final String ACCESSION_RECORD_0_ID = "4a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String ACCESSION_RECORD_1_ID = "5a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String INSTANCE_AUTHOR = "Pratchett, Terry";
  private static final String INSTANCE_TITLE = "Interesting Times";

  private final static ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private final static int PORT = SocketUtils.findAvailableTcpPort();
  private static WireMockServer wireMockServer;
  private String formattedAccessionUrl;


  @BeforeAll
  static void globalSetUp() {
    wireMockServer = new WireMockServer(PORT);
    wireMockServer.start();
  }

  @BeforeEach
  void setUp() {
    globalValue.setOkapiUrl(String.format("http://localhost:%s/", PORT));
    formattedAccessionUrl = String.format(ACCESSION_URL, port);
  }

  @AfterAll
  static void tearDown() {
    wireMockServer.stop();
  }

  @Test
  void testEventsHandling() throws JsonProcessingException {

    Map<String, Object> configs = new HashMap<>(KafkaTestUtils.producerProps(embeddedKafkaBroker));
    Producer<String, String> producer = new DefaultKafkaProducerFactory<>(configs, new StringSerializer(), new StringSerializer())
      .createProducer();

    var originalItem = new Item().withEffectiveLocationId(OLD_EFFECTIVE_LOCATION_ID)
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var newItem = new Item().withEffectiveLocationId(NEW_EFFECTIVE_LOCATION_ID)
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var newItemWithoutRemoteConfig = new Item().withEffectiveLocationId(randomIdAsString())
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var resourceBodyWithRemoteConfig = DomainEvent.of(originalItem, newItem, DomainEventType.UPDATE, "tenant");
    var resourceBodyWithoutRemoteConfig = DomainEvent.of(originalItem, newItemWithoutRemoteConfig, DomainEventType.UPDATE, "tenant");

    var locationMapping = new MappingRecord();
    locationMapping.setFolioLocationId(UUID.fromString(NEW_EFFECTIVE_LOCATION_ID));
    locationMapping.setConfigurationId(UUID.fromString(REMOTE_STORAGE_ID));
    locationMappingsRepository.save(locationMapping);

    producer.send(new ProducerRecord<>(TOPIC, OBJECT_MAPPER.writeValueAsString(resourceBodyWithRemoteConfig)));
    producer.send(new ProducerRecord<>(TOPIC, OBJECT_MAPPER.writeValueAsString(resourceBodyWithoutRemoteConfig)));
    producer.flush();

    await().atMost(Duration.TEN_SECONDS)
      .until(() -> accessionQueueRepository.count() == 1L);

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    verifyCreatedAccessionQueueRecord(actualAccessionQueueRecord);

  }

  @Test
  void shouldFindAccessionQueuesByRemoteStorageId() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = restTemplate.getForEntity(formattedAccessionUrl +"?storageId=" + REMOTE_STORAGE_ID, AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0).getRemoteStorageId(), equalTo(REMOTE_STORAGE_ID));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesByAccessionedFlagTrue() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), true));

    ResponseEntity<AccessionQueues> responseEntity = restTemplate.getForEntity(formattedAccessionUrl + "?accessioned=true", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0).getAccessioned(), equalTo(true));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesByAccessionedFlagFalse() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = restTemplate.getForEntity(formattedAccessionUrl + "?accessioned=false", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(2));
  }

  @Test
  void shouldFindAccessionQueuesWithOffset() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = restTemplate.getForEntity(formattedAccessionUrl + "?offset=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
//    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));  //todo fix for search with only pagination values
  }

  @Test
  void shouldFindAccessionQueuesWithLimit() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = restTemplate.getForEntity(formattedAccessionUrl + "?limit=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
//    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1)); //todo fix for search with only pagination values
  }

  private void createBaseAccessionQueueRecord() {
    AccessionQueueRecord accessionQueueRecord = new AccessionQueueRecord();
    accessionQueueRecord.setId(stringToUUIDSafe(ACCESSION_RECORD_0_ID));
    accessionQueueRepository.save(accessionQueueRecord);
  }

  @Test
  void shouldSetAccessioned() throws JsonProcessingException {
    UUID id = UUID.randomUUID();
    accessionQueueRepository.save(buildAccessionQueueRecord(id, false));

    restTemplate.put(String.format(ACCESSION_URL, port) + "/" + id,null);

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
    assertTrue(actualAccessionQueueRecord.isAccessioned(), StringUtil.EMPTY_STRING);
  }

  @Test
  void shouldThrowNotFoundExceptionWhenIdDoesNotExist() throws JsonProcessingException {
    accessionQueueRepository.save(buildAccessionQueueRecord(UUID.randomUUID(),false));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
        restTemplate.put(String.format(ACCESSION_URL, port) + "/" + UUID.randomUUID(),null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  private AccessionQueueRecord buildAccessionQueueRecord(UUID id, Boolean accessioned) {
    return AccessionQueueRecord.builder()
        .id(id)
        .remoteStorageId(UUID.fromString(REMOTE_STORAGE_ID))
        .accessioned(accessioned)
        .build();
  }

  private static void verifyCreatedAccessionQueueRecord(AccessionQueueRecord accessionQueueRecord) {

    assertThat(accessionQueueRecord.getId(), notNullValue());
    assertThat(accessionQueueRecord.getItemBarcode(), equalTo(BARCODE));
    assertThat(accessionQueueRecord.getCallNumber(), equalTo(CALL_NUMBER));
    assertThat(accessionQueueRecord.getRemoteStorageId(), equalTo(UUID.fromString(REMOTE_STORAGE_ID)));
    assertThat(accessionQueueRecord.getInstanceAuthor(), equalTo(INSTANCE_AUTHOR));
    assertThat(accessionQueueRecord.getInstanceTitle(), equalTo(INSTANCE_TITLE));
  }

}
