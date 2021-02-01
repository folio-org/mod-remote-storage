package org.folio.rs.service;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Objects;
import java.util.UUID;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang.StringUtils;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.repository.AccessionQueueRepository;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;

@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Log4j2
@Sql(executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:ClearTestData.sql")
public class AccessionQueueServiceTest extends TestBase {

  private static final String CALL_NUMBER = "+1-111-22-33";
  private static final String BARCODE = "1234567890";
  private static final String INSTANCE_ID = "a89eccf0-57a6-495e-898d-32b9b2210f2f";
  private static final String OLD_EFFECTIVE_LOCATION_ID = "59a54d96-2563-4f55-a6b0-fb23cb5760af";
  private static final String NEW_EFFECTIVE_LOCATION_ID = "cefb6261-a8d7-44f1-8264-90667d6c45d3";
  private static final String REMOTE_STORAGE_ID = "0f976099-2c7a-48ca-9271-3523905eab6b";
  private static final String INSTANCE_AUTHOR = "Pratchett, Terry";
  private static final String INSTANCE_TITLE = "Interesting Times";
  private static final String ACCESSION_RECORD_0_ID = "4a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String ACCESSION_RECORD_1_ID = "5a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String ACCESSION_URL = "http://localhost:%s/remote-storage/accessions";
  private String formattedAccessionUrl;

  @Autowired
  private AccessionQueueRepository accessionQueueRepository;
  @Autowired
  private LocationMappingsService locationMappingsService;
  @Autowired
  private AccessionQueueService accessionQueueService;

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
  void shouldFindAccessionQueuesByRemoteStorageId() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl +"?storageId=" + REMOTE_STORAGE_ID, AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0).getRemoteStorageId(), equalTo(REMOTE_STORAGE_ID));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesWithAccessionDateTime() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    AccessionQueueRecord accessionQueueRecord = buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID));
    accessionQueueRecord.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(accessionQueueRecord);

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?accessioned=true", AccessionQueues.class);

    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().get(0), notNullValue());
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesWithOffset() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?offset=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
  }

  @Test
  void shouldFindAccessionQueuesWithLimit() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?limit=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
  }

  @Test
  void shouldSetAccessionedById() {
    UUID id = UUID.randomUUID();
    accessionQueueRepository.save(buildAccessionQueueRecord(id));

    put(formattedAccessionUrl + "/id/" + id,null);

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
  }

  @Test
  void shouldSetAccessionedByBarcode() {
    accessionQueueRepository.save(buildAccessionQueueRecord(UUID.randomUUID()));

    put(formattedAccessionUrl + "/barcode/" + BARCODE,null);

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getItemBarcode(), equalTo(BARCODE));
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
  }

  @Test
  void shouldThrowNotFoundExceptionWhenIdDoesNotExist() {
    accessionQueueRepository.save(buildAccessionQueueRecord(UUID.randomUUID()));
    String url = formattedAccessionUrl + "/id/" + UUID.randomUUID();

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
        put(url,null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldThrowNotFoundExceptionWhenAccessionQueueIsAlreadyAccessioned() {
    UUID id = UUID.randomUUID();
    AccessionQueueRecord accessionQueueRecord = buildAccessionQueueRecord(id);
    accessionQueueRecord.setAccessionedDateTime(LocalDateTime.now());
    accessionQueueRepository.save(accessionQueueRecord);
    String url = formattedAccessionUrl + "/id/" + id;

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
        put(url,null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldThrowNotFoundExceptionWhenBarcodeDoesNotExist() {
    accessionQueueRepository.save(buildAccessionQueueRecord(UUID.randomUUID()));
    String url = formattedAccessionUrl + "/barcode/" + UUID.randomUUID();

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
        put(url,null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldFindAccessionQueuesByCreatedDate() {
    LocalDateTime createdDate = LocalDateTime.now();
    AccessionQueueRecord accessionQueueRecord = createBaseAccessionQueueRecord();
    accessionQueueRecord.setCreatedDateTime(createdDate);
    accessionQueueRepository.save(accessionQueueRecord);

    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?createdDate=" + createdDate, AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
  }

  @Test
  void shouldThrowBadRequestExceptionWhenCreateDateHasWrongFormat() {
    LocalDateTime createdDate = LocalDateTime.now();
    AccessionQueueRecord accessionQueueRecord = createBaseAccessionQueueRecord();
    accessionQueueRecord.setCreatedDateTime(createdDate);
    accessionQueueRepository.save(accessionQueueRecord);
    accessionQueueRepository
        .save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
        get(formattedAccessionUrl + "?createdDate=123", AccessionQueues.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  private AccessionQueueRecord createBaseAccessionQueueRecord() {
    AccessionQueueRecord accessionQueueRecord = new AccessionQueueRecord();
    accessionQueueRecord.setId(stringToUUIDSafe(ACCESSION_RECORD_0_ID));
    return accessionQueueRecord;
  }

  private AccessionQueueRecord buildAccessionQueueRecord(UUID id) {
    return AccessionQueueRecord.builder()
        .id(id)
        .remoteStorageId(UUID.fromString(REMOTE_STORAGE_ID))
        .itemBarcode(BARCODE)
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
