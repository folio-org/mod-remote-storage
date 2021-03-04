package org.folio.rs.service;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.repository.RetrievalQueueRepository;
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

import lombok.extern.log4j.Log4j2;

@ExtendWith(SpringExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Log4j2
@Sql(executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD, scripts = "classpath:ClearTestData.sql")
public class RetrievalQueueServiceTest extends TestBase {

  private static final String BARCODE = "1234567890";
  private static final String REMOTE_STORAGE_ID = "0f976099-2c7a-48ca-9271-3523905eab6b";
  private static final String RETRIEVAL_RECORD_0_ID = "4a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String RETRIEVAL_RECORD_1_ID = "5a38cc7d-b8c8-4a43-ad07-14c784dfbcbb";
  private static final String RETRIEVALS_API_URL = "http://localhost:%s/remote-storage/retrievals";
  private String formattedRetrievalUrl;

  @Autowired
  private RetrievalQueueRepository retrievalQueueRepository;

  @BeforeEach
  void prepareUrl() {
    formattedRetrievalUrl = String.format(RETRIEVALS_API_URL, okapiPort);
  }

  @Test
  void shouldFindRetrievalQueuesByRemoteStorageId() {
    retrievalQueueRepository.save(createBaseRetrievalQueueRecord());
    retrievalQueueRepository.save(buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID)));

    ResponseEntity<RetrievalQueues> responseEntity = get(formattedRetrievalUrl + "?storageId=" + REMOTE_STORAGE_ID,
        RetrievalQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getRetrievals()
      .get(0)
      .getRemoteStorageId(), equalTo(REMOTE_STORAGE_ID));
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getRetrievals()
      .size(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getTotalRecords(), equalTo(1));
  }

  @Test
  void shouldFindRetrievalQueuesWithRetrievalDateTime() {
    retrievalQueueRepository.save(createBaseRetrievalQueueRecord());
    var retrievalQueueRecord = buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID));
    retrievalQueueRecord.setRetrievedDateTime(LocalDateTime.now());
    retrievalQueueRepository.save(retrievalQueueRecord);

    ResponseEntity<RetrievalQueues> allRecords = get(formattedRetrievalUrl, RetrievalQueues.class);

    assertThat(Objects.requireNonNull(allRecords.getBody()).getRetrievals(), notNullValue());
    assertThat(Objects.requireNonNull(allRecords.getBody()).getRetrievals().size(), equalTo(2));
    assertThat(Objects.requireNonNull(allRecords.getBody()).getTotalRecords(), equalTo(2));

    ResponseEntity<RetrievalQueues> retrievedRecord = get(formattedRetrievalUrl + "?retrieved=true", RetrievalQueues.class);

    assertThat(Objects.requireNonNull(retrievedRecord.getBody()).getRetrievals().get(0), notNullValue());
    assertThat(Objects.requireNonNull(retrievedRecord.getBody()).getRetrievals().size(), equalTo(1));
    assertThat(Objects.requireNonNull(retrievedRecord.getBody()).getTotalRecords(), equalTo(1));
    assertThat(Objects.requireNonNull(retrievedRecord.getBody()).getRetrievals().get(0).getRetrievedDateTime(), notNullValue());

    ResponseEntity<RetrievalQueues> nonRetrievedRecord = get(formattedRetrievalUrl + "?retrieved=false", RetrievalQueues.class);

    assertThat(Objects.requireNonNull(nonRetrievedRecord.getBody()).getRetrievals().get(0), notNullValue());
    assertThat(Objects.requireNonNull(nonRetrievedRecord.getBody()).getRetrievals().size(), equalTo(1));
    assertThat(Objects.requireNonNull(nonRetrievedRecord.getBody()).getTotalRecords(), equalTo(1));
    assertThat(Objects.requireNonNull(nonRetrievedRecord.getBody()).getRetrievals().get(0).getRetrievedDateTime(), nullValue());
  }

  @Test
  void shouldFindRetrievalQueuesWithOffset() {
    retrievalQueueRepository.save(createBaseRetrievalQueueRecord());
    retrievalQueueRepository.save(buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID)));

    ResponseEntity<RetrievalQueues> responseEntity = get(formattedRetrievalUrl + "?offset=1", RetrievalQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getRetrievals()
      .size(), equalTo(1));
  }

  @Test
  void shouldFindRetrievalQueuesWithLimit() {
    retrievalQueueRepository.save(createBaseRetrievalQueueRecord());
    retrievalQueueRepository.save(buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID)));

    ResponseEntity<RetrievalQueues> responseEntity = get(formattedRetrievalUrl + "?limit=1", RetrievalQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getRetrievals()
      .size(), equalTo(1));
  }

  @Test
  void shouldSetRetrievedById() {
    UUID id = UUID.randomUUID();
    retrievalQueueRepository.save(buildRetrievalQueueRecord(id));

    put(formattedRetrievalUrl + "/id/" + id, null);

    var actualRetrievalQueueRecord = retrievalQueueRepository.findAll()
      .get(0);
    assertThat(actualRetrievalQueueRecord.getRetrievedDateTime(), notNullValue());
  }

  @Test
  void shouldSetRetrievedByBarcode() {
    retrievalQueueRepository.save(buildRetrievalQueueRecord(UUID.randomUUID()));

    put(formattedRetrievalUrl + "/barcode/" + BARCODE, null);

    var actualRetrievalQueueRecord = retrievalQueueRepository.findAll()
      .get(0);
    assertThat(actualRetrievalQueueRecord.getItemBarcode(), equalTo(BARCODE));
    assertThat(actualRetrievalQueueRecord.getRetrievedDateTime(), notNullValue());
  }

  @Test
  void shouldThrowNotFoundExceptionWhenIdDoesNotExist() {
    retrievalQueueRepository.save(buildRetrievalQueueRecord(UUID.randomUUID()));
    String url = formattedRetrievalUrl + "/id/" + UUID.randomUUID();

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> put(url, null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldThrowNotFoundExceptionWhenRetrievalQueueIsAlreadyRetrieved() {
    UUID id = UUID.randomUUID();
    var retrievalQueueRecord = buildRetrievalQueueRecord(id);
    retrievalQueueRecord.setRetrievedDateTime(LocalDateTime.now());
    retrievalQueueRepository.save(retrievalQueueRecord);
    String url = formattedRetrievalUrl + "/id/" + id;

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> put(url, null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldThrowNotFoundExceptionWhenBarcodeDoesNotExist() {
    retrievalQueueRepository.save(buildRetrievalQueueRecord(UUID.randomUUID()));
    var url = formattedRetrievalUrl + "/barcode/" + UUID.randomUUID();

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () -> put(url, null));

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.NOT_FOUND));
  }

  @Test
  void shouldFindRetrievalQueuesByCreatedDate() {
    LocalDateTime createdDate = LocalDateTime.now();
    var retrievalQueueRecord = createBaseRetrievalQueueRecord();
    retrievalQueueRecord.setCreatedDateTime(createdDate);
    retrievalQueueRepository.save(retrievalQueueRecord);

    retrievalQueueRepository.save(buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID)));

    ResponseEntity<RetrievalQueues> responseEntity = get(formattedRetrievalUrl + "?createdDate=" + createdDate,
        RetrievalQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getTotalRecords(), equalTo(1));
    assertThat(Objects.requireNonNull(responseEntity.getBody())
      .getRetrievals()
      .size(), equalTo(1));
  }

  @Test
  void shouldThrowBadRequestExceptionWhenCreateDateHasWrongFormat() {
    LocalDateTime requestDate = LocalDateTime.now();
    var retrievalQueueRecord = createBaseRetrievalQueueRecord();
    retrievalQueueRecord.setCreatedDateTime(requestDate);
    retrievalQueueRepository.save(retrievalQueueRecord);
    retrievalQueueRepository.save(buildRetrievalQueueRecord(stringToUUIDSafe(RETRIEVAL_RECORD_1_ID)));

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class,
        () -> get(formattedRetrievalUrl + "?createdDate=123", RetrievalQueues.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  private RetrievalQueueRecord createBaseRetrievalQueueRecord() {
    var retrievalQueueRecord = new RetrievalQueueRecord();
    retrievalQueueRecord.setId(stringToUUIDSafe(RETRIEVAL_RECORD_0_ID));
    return retrievalQueueRecord;
  }

  private RetrievalQueueRecord buildRetrievalQueueRecord(UUID id) {
    return RetrievalQueueRecord.builder()
      .id(id)
      .remoteStorageId(UUID.fromString(REMOTE_STORAGE_ID))
      .itemBarcode(BARCODE)
      .callNumber("+1-111-222")
      .instanceAuthor("Some Author")
      .instanceTitle("Some title")
      .holdId("hold_id")
      .patronBarcode("987654321")
      .patronName("Some Patron Name")
      .pickupLocation("pickup_location")
      .requestStatus("Request-Status")
      .requestNote("Request_Note")
      .build();
  }

}
