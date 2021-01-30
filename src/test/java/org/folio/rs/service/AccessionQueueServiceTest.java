package org.folio.rs.service;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Collections;
import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.repository.AccessionQueueRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;

@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Log4j2
public class AccessionQueueServiceTest extends TestBase {

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

  private static final String ACCESSION_RECORD_0_ID = "4a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String ACCESSION_RECORD_1_ID = "5a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";

  private static final String ACCESSION_URL = "http://localhost:%s/remote-storage/accessions";

  private String formattedAccessionUrl;


  @BeforeEach
  void prepareUrl() {
    formattedAccessionUrl = String.format(ACCESSION_URL, okapiPort);
  }


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
  @Test
  void shouldFindAccessionQueuesByRemoteStorageId() throws JsonProcessingException {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl +"?storageId=" + REMOTE_STORAGE_ID, AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0).getRemoteStorageId(), equalTo(REMOTE_STORAGE_ID));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesByAccessionedFlagTrue() {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), true));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?accessioned=true", AccessionQueues.class);
    //assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0).getAccessioned(), equalTo(true)); //TODO
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesByAccessionedFlagFalse() {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?accessioned=false", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(2));
  }

  @Test
  void shouldFindAccessionQueuesWithOffset() {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?offset=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    //    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));  //todo fix for search with only pagination values
  }

  @Test
  void shouldFindAccessionQueuesWithLimit() {
    createBaseAccessionQueueRecord();
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID), false));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?limit=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    //    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1)); //todo fix for search with only pagination values
  }

  private void createBaseAccessionQueueRecord() {
    AccessionQueueRecord accessionQueueRecord = new AccessionQueueRecord();
    accessionQueueRecord.setId(stringToUUIDSafe(ACCESSION_RECORD_0_ID));
    accessionQueueRepository.save(accessionQueueRecord);
  }

  @Test
  void shouldSetAccessioned() {
    UUID id = UUID.randomUUID();
    accessionQueueRepository.save(buildAccessionQueueRecord(id, false));

    //put(formattedAccessionUrl + "/" + id,null); //TODO

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
//    assertTrue(actualAccessionQueueRecord.isAccessioned(), StringUtil.EMPTY_STRING);
  }

  @Test
  void shouldThrowNotFoundExceptionWhenIdDoesNotExist() {
    accessionQueueRepository.save(buildAccessionQueueRecord(UUID.randomUUID(),false));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      put(formattedAccessionUrl + "/" + UUID.randomUUID(),null));

    assertThat(exception.getStatusCode(), is(HttpStatus.NOT_FOUND));
  }

  private AccessionQueueRecord buildAccessionQueueRecord(UUID id, Boolean accessioned) {
    return AccessionQueueRecord.builder()
      .id(id)
      .remoteStorageId(UUID.fromString(REMOTE_STORAGE_ID))
//      .accessioned(accessioned) //TODO
      .build();
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
