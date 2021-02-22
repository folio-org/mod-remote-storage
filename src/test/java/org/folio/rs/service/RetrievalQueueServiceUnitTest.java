package org.folio.rs.service;

import static org.folio.rs.util.JsonUtils.readJson;
import static org.folio.rs.util.MapperUtils.mapJsonToMovedEventRequest;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import javax.persistence.EntityNotFoundException;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.Contributor;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Instance;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.MovedEventRequest;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class RetrievalQueueServiceUnitTest {

  private static final String REQUEST_MOVED_PAYLOAD_JSON = "payloads/request_moved.json";
  private static final String ITEM_BARCODE = "653285216743";
  private static final String PATRON_BARCODE = "543285216734";
  private static final String CALL_NUMBER = "1234567890";
  private static final String LOCATION_ID = "53cf956f-c1df-410b-8bea-27f721cca8c0";
  private static final String REQUESTER_ID = "e546d50a-926a-421f-8400-a041a2e9db79";
  private static final String HOLD_ID = "7babb1ab-f46f-4c74-8950-bda779440f6f";
  private static final String INSTANCE_ID = "6abcc1ab-f46f-3c47-8950-bda779440f5d";
  private static final String PICKUP_LOCATION = "3a40852d-49fd-4df2-a1f9-6e2641a6e91f";
  private static final String REMOTE_STORAGE_ID = "d3cfdb9e-e364-4209-85e5-8f49fc0969b5";
  private static final String PATRON_NAME = "John Smith";
  private static final String INSTANCE_AUTHOR = "Matt Gordon";
  private static final String STATUS = "Open - Not yet filled";
  private static final String REQUEST_NOTE = "request note";
  private static final String INSTANCE_TITLE = "Title";

  @InjectMocks
  private RetrievalQueueService service;
  @Captor
  private ArgumentCaptor<RetrievalQueueRecord> captor;
  @Mock
  private RetrievalQueueRepository retrievalQueueRepository;
  @Mock
  private LocationMappingsService locationMappingsService;
  @Mock
  private InventoryClient inventoryClient;
  @Mock
  private UsersClient usersClient;
  @Mock
  private Item item;
  @Mock
  private ResultList<Item> items;
  @Mock
  private LocationMapping locationMapping;
  @Mock
  private ResultList<User> users;
  @Mock
  private User user;
  @Mock
  private EffectiveCallNumberComponents callNumberComponents;
  @Mock
  private ResultList<Instance> instances;
  @Mock
  private Contributor contributor;
  @Mock
  private Instance instance;

  private MovedEventRequest request;

  @BeforeEach
  void prepareTestData() {
    when(inventoryClient.getItem("barcode==" + ITEM_BARCODE)).thenReturn(items);
    when(items.getResult()).thenReturn(Collections.singletonList(item));
    when(inventoryClient.getInstance("id==" + INSTANCE_ID)).thenReturn(instances);
    when(instances.getResult()).thenReturn(Collections.singletonList(instance));
    when(instance.getTitle()).thenReturn(INSTANCE_TITLE);
    when(instance.getContributors()).thenReturn(Collections.singletonList(contributor));
    when(contributor.getName()).thenReturn(INSTANCE_AUTHOR);
    when(item.getEffectiveLocationId()).thenReturn(LOCATION_ID);
    when(item.getEffectiveCallNumberComponents()).thenReturn(callNumberComponents);
    when(item.getInstanceId()).thenReturn(INSTANCE_ID);
    when(callNumberComponents.getCallNumber()).thenReturn(CALL_NUMBER);
    when(locationMappingsService.getMappingByFolioLocationId(LOCATION_ID)).thenReturn(locationMapping);
    when(locationMapping.getConfigurationId()).thenReturn(REMOTE_STORAGE_ID);
    when(usersClient.query("id==" + REQUESTER_ID)).thenReturn(users);
    when(users.getResult()).thenReturn(Collections.singletonList(user));
    when(user.getBarcode()).thenReturn(PATRON_BARCODE);
    when(user.getUsername()).thenReturn(PATRON_NAME);
    this.request = mapJsonToMovedEventRequest(readJson(REQUEST_MOVED_PAYLOAD_JSON));
  }

  @Test
  void shouldSaveMovedEventRequest() {
    service.processMovedEventRequest(request);

    verify(retrievalQueueRepository, times(1)).save(captor.capture());
    RetrievalQueueRecord record = captor.getValue();
    assertEquals(HOLD_ID, record.getHoldId());
    assertEquals(ITEM_BARCODE, record.getItemBarcode());
    assertEquals(CALL_NUMBER, record.getCallNumber());
    assertEquals(PATRON_BARCODE, record.getPatronBarcode());
    assertEquals(PATRON_NAME, record.getPatronName());
    assertEquals(PICKUP_LOCATION, record.getPickupLocation());
    assertEquals(STATUS, record.getRequestStatus());
    assertEquals(REQUEST_NOTE, record.getRequestNote());
    assertEquals(INSTANCE_TITLE, record.getInstanceTitle());
    assertEquals(INSTANCE_AUTHOR, record.getInstanceAuthor());
    assertEquals(REMOTE_STORAGE_ID, record.getRemoteStorageId().toString());
    assertNotNull(record.getCreatedDateTime());
  }

  @Test
  void shouldNotSaveRequestWhenRequestIsNotPaged() {
    request.setItemStatusName("Hold");

    service.processMovedEventRequest(request);

    verify(retrievalQueueRepository, never()).save(any());
  }

  @Test()
  void shouldThrowExceptionWhenItemByBarcodeIsNotFound() {
    when(inventoryClient.getItem("barcode==" + ITEM_BARCODE)).thenReturn(null);

    assertThrows(EntityNotFoundException.class, () -> service.processMovedEventRequest(request));
  }

  @Test
  void shouldNotProcessRecordsWhenLocationIsNotRemote() {
    when(locationMappingsService.getMappingByFolioLocationId(LOCATION_ID)).thenReturn(null);

    service.processMovedEventRequest(request);

    verify(retrievalQueueRepository, never()).save(any());
  }

  @Test
  void shouldThrowExceptionWhenPatronIsNotFound() {
    when(usersClient.query("id==" + REQUESTER_ID)).thenReturn(null);

    assertThrows(EntityNotFoundException.class, () -> service.processMovedEventRequest(request));
  }

  @Test
  void shouldThrowExceptionWhenInstanceIsNotFound() {
    when(inventoryClient.getInstance("id==" + INSTANCE_ID)).thenReturn(null);

    assertThrows(EntityNotFoundException.class, () -> service.processMovedEventRequest(request));

  }
}
