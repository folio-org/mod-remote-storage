package org.folio.rs.integration;

import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;

import java.util.Collections;
import java.util.UUID;

import lombok.extern.log4j.Log4j2;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.repository.AccessionQueueRepository;
import org.folio.rs.service.AccessionQueueService;
import org.folio.rs.service.LocationMappingsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@TestPropertySource("classpath:application-test.properties")
@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Log4j2
public class KafkaIntegrationTest extends TestBase {

  @Autowired
  private KafkaMessageListener messageListener;
  @Autowired
  private AccessionQueueRepository accessionQueueRepository;
  @Autowired
  private LocationMappingsService locationMappingsService;
  @Autowired
  private AccessionQueueService accessionQueueService;

  public static final String CALL_NUMBER = "+1-111-22-33";
  public static final String BARCODE = "1234567890";

  public static final String INSTANCE_ID = "a89eccf0-57a6-495e-898d-32b9b2210f2f";
  public static final String OLD_EFFECTIVE_LOCATION_ID = "59a54d96-2563-4f55-a6b0-fb23cb5760af";
  public static final String NEW_EFFECTIVE_LOCATION_ID = "cefb6261-a8d7-44f1-8264-90667d6c45d3";
  public static final String REMOTE_STORAGE_ID = "0f976099-2c7a-48ca-9271-3523905eab6b";

  public static final String INSTANCE_AUTHOR = "Pratchett, Terry";
  public static final String INSTANCE_TITLE = "Interesting Times";


  @Test
  void testItemUpdatingEventHandling() {

    var locationMapping = new LocationMapping();
    locationMapping.setFolioLocationId(NEW_EFFECTIVE_LOCATION_ID);
    locationMapping.setConfigurationId(REMOTE_STORAGE_ID);
    locationMappingsService.postMapping(locationMapping);

    var originalItem = new Item().withEffectiveLocationId(OLD_EFFECTIVE_LOCATION_ID)
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(
        new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var newItem = new Item().withEffectiveLocationId(NEW_EFFECTIVE_LOCATION_ID)
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(
        new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var newItemWithoutRemoteConfig = new Item().withEffectiveLocationId(randomIdAsString())
      .withInstanceId(INSTANCE_ID)
      .withBarcode(BARCODE)
      .withEffectiveCallNumberComponents(
        new EffectiveCallNumberComponents().withCallNumber(CALL_NUMBER));

    var resourceBodyWithRemoteConfig = DomainEvent
      .of(originalItem, newItem, DomainEventType.UPDATE, TEST_TENANT);
    var resourceBodyWithoutRemoteConfig = DomainEvent
      .of(originalItem, newItemWithoutRemoteConfig, DomainEventType.UPDATE,
        TEST_TENANT);

    accessionQueueService.processAccessionQueueRecord(
      Collections.singletonList(resourceBodyWithRemoteConfig));
    accessionQueueService.processAccessionQueueRecord(
      Collections.singletonList(resourceBodyWithoutRemoteConfig));

    var actualAccessionQueueRecord = accessionQueueRepository.findAll()
      .get(0);
    verifyCreatedAccessionQueueRecord(actualAccessionQueueRecord);

  }

  private static void verifyCreatedAccessionQueueRecord(AccessionQueueRecord accessionQueueRecord) {
    assertThat(accessionQueueRecord.getId(), notNullValue());
    assertThat(accessionQueueRecord.getItemBarcode(), equalTo(BARCODE));
    assertThat(accessionQueueRecord.getCallNumber(), equalTo(CALL_NUMBER));
    assertThat(accessionQueueRecord.getRemoteStorageId(),
      equalTo(UUID.fromString(REMOTE_STORAGE_ID)));
    assertThat(accessionQueueRecord.getInstanceAuthor(), equalTo(INSTANCE_AUTHOR));
    assertThat(accessionQueueRecord.getInstanceTitle(), equalTo(INSTANCE_TITLE));
  }

}
