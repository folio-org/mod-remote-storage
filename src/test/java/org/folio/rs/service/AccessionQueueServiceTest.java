package org.folio.rs.service;

import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;
import static org.folio.rs.util.Utils.randomIdAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Pattern;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.http.RequestMethod;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang.StringUtils;
import org.folio.rs.TestBase;
import org.folio.rs.domain.dto.AccessionQueue;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.AccessionRequest;
import org.folio.rs.domain.dto.AccessionWorkflowDetails;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.DomainEventType;
import org.folio.rs.domain.dto.HoldingsRecord;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.ItemsMove;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.TimeUnits;
import org.folio.rs.domain.entity.AccessionQueueRecord;
import org.folio.rs.repository.AccessionQueueRepository;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
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
  private static final String REMOTE_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f712cca7c0";
  private static final String ITEM_STORAGE_ITEMS = "/inventory/items/";
  private static final Pattern HOLDINGS_STORAGE_HOLDINGS_PATTERN = Pattern
    .compile("/holdings-storage/holdings/[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}");
  private String formattedAccessionUrl;

  @Autowired
  private AccessionQueueRepository accessionQueueRepository;
  @Autowired
  private LocationMappingsService locationMappingsService;
  @Autowired
  private AccessionQueueService accessionQueueService;
  @Autowired
  private ConfigurationsService configurationsService;

  @BeforeEach
  void prepareUrl() {
    formattedAccessionUrl = String.format(ACCESSION_URL, okapiPort);
  }


  @Test
  void testItemUpdatingEventHandling() {

    var mapping = new RemoteLocationConfigurationMapping()
      .folioLocationId(NEW_EFFECTIVE_LOCATION_ID)
      .configurationId(REMOTE_STORAGE_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(mapping);

    var originalItem = new Item().effectiveLocationId(OLD_EFFECTIVE_LOCATION_ID)
      .instanceId(INSTANCE_ID)
      .barcode(BARCODE)
      .effectiveCallNumberComponents(
        new ItemEffectiveCallNumberComponents().callNumber(CALL_NUMBER));

    var newItem = new Item().effectiveLocationId(NEW_EFFECTIVE_LOCATION_ID)
      .instanceId(INSTANCE_ID)
      .barcode(BARCODE)
      .effectiveCallNumberComponents(
        new ItemEffectiveCallNumberComponents().callNumber(CALL_NUMBER));

    var newItemWithoutRemoteConfig = new Item().effectiveLocationId(randomIdAsString())
      .instanceId(INSTANCE_ID)
      .barcode(BARCODE)
      .effectiveCallNumberComponents(
        new ItemEffectiveCallNumberComponents().callNumber(CALL_NUMBER));

    var resourceBodyWithRemoteConfig = DomainEvent
      .of(originalItem, newItem, DomainEventType.UPDATE, TEST_TENANT);
    var resourceBodyWithoutRemoteConfig = DomainEvent
      .of(originalItem, newItemWithoutRemoteConfig, DomainEventType.UPDATE,
        TEST_TENANT);

    accessionQueueService.processAccessionQueueRecord(
      Collections.singletonList(resourceBodyWithRemoteConfig));
    accessionQueueService.processAccessionQueueRecord(
      Collections.singletonList(resourceBodyWithoutRemoteConfig));

    AccessionQueueRecord accessionQueueRecord = new AccessionQueueRecord();
    accessionQueueRecord.setItemBarcode(BARCODE);
    verifyCreatedAccessionQueueRecord(accessionQueueRepository.findAll(Example.of(accessionQueueRecord)).get(0));

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

    ResponseEntity<AccessionQueues> allAccessionQueueRecords = get(formattedAccessionUrl, AccessionQueues.class);

    assertThat(Objects.requireNonNull(allAccessionQueueRecords.getBody()).getAccessions(), notNullValue());
    assertThat(Objects.requireNonNull(allAccessionQueueRecords.getBody()).getAccessions().size(), equalTo(3));
    assertThat(Objects.requireNonNull(allAccessionQueueRecords.getBody()).getTotalRecords(), equalTo(3));

    ResponseEntity<AccessionQueues> accessionedRecords = get(formattedAccessionUrl + "?accessioned=true", AccessionQueues.class);

    assertThat(Objects.requireNonNull(accessionedRecords.getBody()).getAccessions().get(0), notNullValue());
    assertThat(Objects.requireNonNull(accessionedRecords.getBody()).getAccessions().size(), equalTo(2));
    assertThat(Objects.requireNonNull(accessionedRecords.getBody()).getTotalRecords(), equalTo(2));
    assertThat(Objects.requireNonNull(accessionedRecords.getBody()).getAccessions().get(0).getAccessionedDateTime(), notNullValue());

    ResponseEntity<AccessionQueues> notAccessionedRecords = get(formattedAccessionUrl + "?accessioned=false", AccessionQueues.class);
    assertThat(Objects.requireNonNull(notAccessionedRecords.getBody()).getAccessions().get(0), notNullValue());
    assertThat(Objects.requireNonNull(notAccessionedRecords.getBody()).getAccessions().size(), equalTo(1));
    assertThat(Objects.requireNonNull(notAccessionedRecords.getBody()).getTotalRecords(), equalTo(1));
    assertThat(Objects.requireNonNull(notAccessionedRecords.getBody()).getAccessions().get(0).getAccessionedDateTime(), nullValue());

  }

  @Test
  void shouldFindAccessionQueuesWithOffset() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?offset=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(3));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(2));
  }

  @Test
  void shouldFindAccessionQueuesWithLimit() {
    accessionQueueRepository.save(createBaseAccessionQueueRecord());
    accessionQueueRepository.save(buildAccessionQueueRecord(stringToUUIDSafe(ACCESSION_RECORD_1_ID)));

    ResponseEntity<AccessionQueues> responseEntity = get(formattedAccessionUrl + "?limit=1", AccessionQueues.class);
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getTotalRecords(), equalTo(3));
    assertThat(Objects.requireNonNull(responseEntity.getBody()).getAccessions().size(), equalTo(1));
  }

  @Test
  void shouldSetAccessionedById() {
    UUID id = UUID.randomUUID();
    accessionQueueRepository.save(buildAccessionQueueRecord(id));

    put(formattedAccessionUrl + "/id/" + id,null);

    var actualAccessionQueueRecord = accessionQueueRepository.findById(id).get();
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
  }

  @Test
  void shouldSetAccessionedByBarcode() {
    UUID uuid = UUID.randomUUID();
    accessionQueueRepository.save(buildAccessionQueueRecord(uuid));

    put(formattedAccessionUrl + "/barcode/" + BARCODE,null);

    var actualAccessionQueueRecord = accessionQueueRepository.findById(uuid).get();
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

  @Test
  void shouldChangeItemPermanentLocationIfHoldingHasTheSameOne() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(REMOTE_STORAGE_ID)
      .folioLocationId(REMOTE_LOCATION_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("38268030");

    ResponseEntity<AccessionQueue> response = post(formattedAccessionUrl, accessionRequest, AccessionQueue.class);
    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());
    assertThatItemPermanentLocationWasChanged("a31301dc-0a28-49e6-9fa2-499e07c0bb42", REMOTE_LOCATION_ID);
    assertThat(response.getBody().getAccessionedDateTime(), notNullValue());
    assertThat(response.getBody().getPermanentLocationId(), equalTo(REMOTE_LOCATION_ID));
    assertThat(response.getBody().getRemoteStorageId(), equalTo(accessionRequest.getRemoteStorageId()));
  }

  @Test
  void shouldChangeHoldingPermanentLocationIfAllItemsWithinHoldingHasRemoteLocation() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(REMOTE_STORAGE_ID)
      .folioLocationId(REMOTE_LOCATION_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("38268031");

    post(formattedAccessionUrl, accessionRequest, AccessionQueue.class);
    assertThatHoldingPermanentLocationWasChanged(REMOTE_LOCATION_ID);
  }

  @Test
  void shouldMoveItemToHoldingWithRemoteLocationIfItExists() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.DUPLICATE_HOLDINGS);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(REMOTE_STORAGE_ID)
      .folioLocationId(REMOTE_LOCATION_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("38268032");

    post(formattedAccessionUrl, accessionRequest, AccessionQueue.class);
    assertThatItemWasMovedToHoldingWithRemoteLocation("336034b4-0524-45d7-b778-c769274baccf",
      "a31301dc-0a28-49e6-9fa2-499e07c0bb42");
  }

  @Test
  void shouldChangeHoldingsLocationAndSetLocationsToItemsWhenChangePermanentLocationSelected() {
    var remoteStorageId = UUID.randomUUID().toString();
    var remoteLocationId = UUID.randomUUID().toString();

    var storageConfiguration = new StorageConfiguration()
      .id(remoteStorageId)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.CHANGE_PERMANENT_LOCATION);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(remoteStorageId)
      .folioLocationId(remoteLocationId);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(remoteStorageId)
      .itemBarcode("111");

    ResponseEntity<AccessionQueue> response = post(formattedAccessionUrl, accessionRequest, AccessionQueue.class);

    var actualAccessionQueueRecord = accessionQueueRepository.findAll().get(0);
    assertThat(actualAccessionQueueRecord.getAccessionedDateTime(), notNullValue());

    assertThatHoldingPermanentLocationWasChanged(remoteLocationId);

    // accessioned item's permanent location should be changed to remote
    assertThatItemPermanentLocationWasChanged("0b96a642-5e7f-452d-9cae-9cee66c9a892", remoteLocationId);

    // item's permanent location should be changed to holdings record's original location
    // if item contains neither permanent or temporary location
    assertThatItemPermanentLocationWasChanged("2b04ef41-b08d-432a-b27f-b434655b5aff", "d9cd0bed-1b49-4b5e-a7bd-064b8d177231");

    // item containing either permanent or temporary location should not be modified
    assertThereWereNoPutItemRequests("fc90dd2b-06d5-4f23-a8b9-e26ede8e4d03");
  }

  @Test
  void shouldDuplicateHoldingsRecordAndMoveItemIfDuplicateHoldingsSelectedAndNoHoldingExists() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.DUPLICATE_HOLDINGS);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(REMOTE_STORAGE_ID)
      .folioLocationId(REMOTE_LOCATION_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("0001");

    post(formattedAccessionUrl, accessionRequest, AccessionQueue.class);
    assertThatHoldingsRecordWasCreated();
    assertThatItemWasMovedToHoldingWithRemoteLocation("7f806ac6-a0ef-4962-95f8-0b3154b70a08",
      "fd14f552-2f2f-48a4-a777-570d087ba224");
  }

  @Test
  void shouldRespondWithBadRequestIfConfigurationHasNoAccessionDetails() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES);
    configurationsService.postConfiguration(storageConfiguration);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("38268032");

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      post(formattedAccessionUrl, accessionRequest, AccessionQueue.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldRespondWithBadRequestIfThereAreMultipleHoldingsWithSameRemoteLocationExist() {
    var storageConfiguration = new StorageConfiguration()
      .id(REMOTE_STORAGE_ID)
      .name("Test")
      .providerName("CAIA_SOFT")
      .accessionDelay(1)
      .accessionTimeUnit(TimeUnits.MINUTES)
      .accessionWorkflowDetails(AccessionWorkflowDetails.DUPLICATE_HOLDINGS);
    configurationsService.postConfiguration(storageConfiguration);

    var locationMapping = new RemoteLocationConfigurationMapping()
      .configurationId(REMOTE_STORAGE_ID)
      .folioLocationId(REMOTE_LOCATION_ID);
    locationMappingsService.postRemoteLocationConfigurationMapping(locationMapping);

    var accessionRequest = new AccessionRequest()
      .remoteStorageId(REMOTE_STORAGE_ID)
      .itemBarcode("0003");

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      post(formattedAccessionUrl, accessionRequest, AccessionQueue.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldRespondWithBadRequestIfRemoteStorageIdDoesNotExist() {
    var accessionRequest = new AccessionRequest()
      .remoteStorageId("9b26007e-9df2-4c9c-a6d2-bdd3c93de6c9")
      .itemBarcode("12345");

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      post(formattedAccessionUrl, accessionRequest, AccessionQueue.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  @Test
  void shouldRespondWithUnprocessableEntityIfRemoteStorageIdIsInvalid() {
    var accessionRequest = new AccessionRequest()
      .remoteStorageId("invalid-uuid")
      .itemBarcode("12345");

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      post(formattedAccessionUrl, accessionRequest, AccessionQueue.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.UNPROCESSABLE_ENTITY));
  }


  @Test
  void shouldRespondWithBadRequestIfItemWasNotFound() {
    var accessionRequest = buildAccessionRequest("not-exist-item-barcode");

    HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, () ->
      post(formattedAccessionUrl, accessionRequest, AccessionQueue.class), StringUtils.EMPTY);

    assertThat(exception.getStatusCode(), Matchers.is(HttpStatus.BAD_REQUEST));
  }

  private AccessionRequest buildAccessionRequest(String itemBarcode) {
    return new AccessionRequest()
      .remoteStorageId("de17bad7-2a30-4f1c-bee5-f653ded15629")
      .itemBarcode(itemBarcode);
  }

  @SneakyThrows
  private void assertThatItemPermanentLocationWasChanged(String itemId, String desiredLocationId) {
    var requestBody = wireMockServer.getAllServeEvents().stream()
      .filter(e -> RequestMethod.PUT.equals(e.getRequest().getMethod()) &&
        ITEM_STORAGE_ITEMS.concat(itemId).equals(e.getRequest().getUrl()))
      .findFirst()
      .get()
      .getRequest()
      .getBody();
    ObjectMapper mapper = new ObjectMapper();
    var item = mapper.readValue(requestBody, Item.class);
    assertThat(item.getPermanentLocation().getId(), equalTo(desiredLocationId));
  }

  @SneakyThrows
  private void assertThereWereNoPutItemRequests(String itemId) {
    var putRequest = wireMockServer.getAllServeEvents().stream()
      .filter(e -> RequestMethod.PUT.equals(e.getRequest().getMethod()) &&
        ITEM_STORAGE_ITEMS.concat(itemId).equals(e.getRequest().getUrl()))
      .findFirst();
    assertThat(putRequest, equalTo(Optional.empty()));
  }

  @SneakyThrows
  private void assertThatHoldingPermanentLocationWasChanged(String desiredLocationId) {
    var requestBody = wireMockServer.getAllServeEvents().stream()
      .filter(e -> RequestMethod.PUT.equals(e.getRequest().getMethod()) &&
        HOLDINGS_STORAGE_HOLDINGS_PATTERN.matcher(e.getRequest().getUrl()).matches())
      .findFirst()
      .get()
      .getRequest()
      .getBody();
    ObjectMapper mapper = new ObjectMapper();
    var holding = mapper.readValue(requestBody, HoldingsRecord.class);
    assertThat(holding.getPermanentLocationId(), equalTo(desiredLocationId));
  }

  @SneakyThrows
  private void assertThatItemWasMovedToHoldingWithRemoteLocation(String holdingsRecordId, String itemId) {
    var requestBody = wireMockServer.getAllServeEvents().stream()
      .filter(e -> RequestMethod.POST.equals(e.getRequest().getMethod()) &&
        "/inventory/items/move".equals(e.getRequest().getUrl()))
      .findFirst()
      .get()
      .getRequest()
      .getBody();
    ObjectMapper mapper = new ObjectMapper();
    var itemsMoveRequest = mapper.readValue(requestBody, ItemsMove.class);
    assertThat(itemsMoveRequest.getToHoldingsRecordId(), equalTo(holdingsRecordId));
    assertThat(itemsMoveRequest.getItemIds().size(), equalTo(1));
    assertThat(itemsMoveRequest.getItemIds().get(0), equalTo(itemId));
  }

  @SneakyThrows
  private void assertThatHoldingsRecordWasCreated() {
    var requestBody = wireMockServer.getAllServeEvents().stream()
      .filter(e -> RequestMethod.POST.equals(e.getRequest().getMethod()) &&
        "/holdings-storage/holdings".equals(e.getRequest().getUrl()))
      .findFirst()
      .get()
      .getRequest()
      .getBody();
    ObjectMapper mapper = new ObjectMapper();
    var postHoldingRequest = mapper.readValue(requestBody, HoldingsRecord.class);
    assertThat(postHoldingRequest.getPermanentLocationId(), equalTo(REMOTE_LOCATION_ID));
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
