package org.folio.rs.integration;

import static org.awaitility.Awaitility.await;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.serialization.StringSerializer;
import org.awaitility.Duration;
import org.folio.rs.controller.ControllerTestBase;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.entity.DomainEvent;
import org.folio.rs.domain.entity.DomainEventType;
import org.folio.rs.domain.entity.GlobalValue;
import org.folio.rs.domain.entity.MappingRecord;
import org.folio.rs.dto.EffectiveCallNumberComponents;
import org.folio.rs.dto.Item;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.repository.LocationMappingsRepository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.test.EmbeddedKafkaBroker;
import org.springframework.kafka.test.context.EmbeddedKafka;
import org.springframework.kafka.test.utils.KafkaTestUtils;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.util.SocketUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;

@EmbeddedKafka(topics = { "inventory.items" }, ports = { 9092 })
@TestPropertySource("classpath:application-test.properties")
@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class KafkaIntegrationTest extends ControllerTestBase {

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

  public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private final static int PORT = SocketUtils.findAvailableTcpPort();

  private static final String TOPIC = "inventory.items";

  public static final String CALL_NUMBER = "+1-111-22-33";
  public static final String BARCODE = "1234567890";
  public static WireMockServer wireMockServer;

  public static final String INSTANCE_ID = "a89eccf0-57a6-495e-898d-32b9b2210f2f";
  public static final String OLD_EFFECTIVE_LOCATION_ID = "59a54d96-2563-4f55-a6b0-fb23cb5760af";
  public static final String NEW_EFFECTIVE_LOCATION_ID = "cefb6261-a8d7-44f1-8264-90667d6c45d3";
  public static final String REMOTE_STORAGE_ID = "0f976099-2c7a-48ca-9271-3523905eab6b";

  public static final String INSTANCE_AUTHOR = "Pratchett, Terry";
  public static final String INSTANCE_TITLE = "Interesting Times";

  @BeforeAll
  static void globalSetUp() {
    wireMockServer = new WireMockServer(PORT);
    wireMockServer.start();
  }

  @BeforeEach
  void setUp() {
    globalValue.setOkapiUrl(String.format("http://localhost:%s/", PORT));
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

  private static void verifyCreatedAccessionQueueRecord(AccessionQueueRecord accessionQueueRecord) {

    assertThat(accessionQueueRecord.getId(), notNullValue());
    assertThat(accessionQueueRecord.getItemBarcode(), equalTo(BARCODE));
    assertThat(accessionQueueRecord.getCallNumber(), equalTo(CALL_NUMBER));
    assertThat(accessionQueueRecord.getRemoteStorageId(), equalTo(UUID.fromString(REMOTE_STORAGE_ID)));
    assertThat(accessionQueueRecord.getInstanceAuthor(), equalTo(INSTANCE_AUTHOR));
    assertThat(accessionQueueRecord.getInstanceTitle(), equalTo(INSTANCE_TITLE));
  }

}
