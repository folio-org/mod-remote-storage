package org.folio.rs.service;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import javax.persistence.EntityNotFoundException;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemContributorNames;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.ItemEffectiveLocation;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
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
public class ReturnRetrievalQueueServiceUnitTest {

  private static final String ITEM_BARCODE = "653285216743";
  private static final String PATRON_BARCODE = "543285216734";
  private static final String CALL_NUMBER = "1234567890";
  private static final String EFFECTIVE_LOCATION_ID = "53cf956f-c1df-410b-8bea-27f721cca8c0";
  private static final String PICKUP_SERVICE_POINT_ID = "3a40852d-49fd-4df2-a1f9-6e2641a6e92f";
  private static final String REQUESTER_ID = "e546d50a-926a-421f-8400-a041a2e9db79";
  private static final String HOLD_ID = "7babb1ab-f46f-4c74-8950-bda779440f6f";
  private static final String REMOTE_STORAGE_ID = "d3cfdb9e-e364-4209-85e5-8f49fc0969b5";
  private static final String PATRON_NAME = "John Smith";
  private static final String INSTANCE_AUTHOR = "Matt Gordon";
  private static final String STATUS = "Open - Not yet filled";
  private static final String REQUEST_NOTE = "request note";
  private static final String INSTANCE_TITLE = "Title";
  private static final String REQUEST_TYPE = "PYR";
  private static final String PICKUP_SERVICE_POINT_CODE = "cd1";

  @InjectMocks
  private ReturnRetrievalQueueService service;
  @Captor
  private ArgumentCaptor<ReturnRetrievalQueueRecord> captor;
  @Mock
  private ReturnRetrievalQueueRepository returnRetrievalQueueRepository;
  @Mock
  private LocationMappingsService locationMappingsService;
  @Mock
  private InventoryClient inventoryClient;
  @Mock
  private UsersClient usersClient;
  @Mock
  private ServicePointsClient servicePointsClient;
  @Mock
  private Item item;
  @Mock
  private ResultList<Item> items;
  @Mock
  private ItemContributorNames contributor;
  @Mock
  private LocationMapping locationMapping;
  @Mock
  private ItemEffectiveLocation effectiveLocation;
  @Mock
  private PickupServicePoint pickupServicePoint;
  @Mock
  private ResultList<User> users;
  @Mock
  private User user;
  @Mock
  private ItemEffectiveCallNumberComponents callNumberComponents;
  @Mock
  private RequestEvent requestEvent;


  @BeforeEach
  void prepareTestData() {
    when(requestEvent.getItemBarCode()).thenReturn(ITEM_BARCODE);
    when(requestEvent.getRequesterId()).thenReturn(REQUESTER_ID);
    when(requestEvent.getPickupServicePointId()).thenReturn(PICKUP_SERVICE_POINT_ID);
    when(requestEvent.getHoldId()).thenReturn(HOLD_ID);
    when(requestEvent.getRequestNote()).thenReturn(REQUEST_NOTE);
    when(requestEvent.getRequestStatus()).thenReturn(STATUS);
    when(inventoryClient.getItemsByQuery("barcode==" + ITEM_BARCODE)).thenReturn(items);
    when(items.getResult()).thenReturn(Collections.singletonList(item));
    when(item.getTitle()).thenReturn(INSTANCE_TITLE);
    when(item.getContributorNames()).thenReturn(Collections.singletonList(contributor));
    when(contributor.getName()).thenReturn(INSTANCE_AUTHOR);
    when(item.getEffectiveLocation()).thenReturn(effectiveLocation);
    when(effectiveLocation.getId()).thenReturn(EFFECTIVE_LOCATION_ID);
    when(item.getEffectiveCallNumberComponents()).thenReturn(callNumberComponents);
    when(callNumberComponents.getCallNumber()).thenReturn(CALL_NUMBER);
    when(locationMappingsService.getLocationMapping(EFFECTIVE_LOCATION_ID)).thenReturn(locationMapping);
    when(locationMapping.getRemoteConfigurationId()).thenReturn(REMOTE_STORAGE_ID);
    when(servicePointsClient.getServicePoint(PICKUP_SERVICE_POINT_ID)).thenReturn(pickupServicePoint);
    when(pickupServicePoint.getCode()).thenReturn(PICKUP_SERVICE_POINT_CODE);
    when(usersClient.getUsersByQuery("id==" + REQUESTER_ID)).thenReturn(users);
    when(users.getResult()).thenReturn(Collections.singletonList(user));
    when(user.getBarcode()).thenReturn(PATRON_BARCODE);
    when(user.getUsername()).thenReturn(PATRON_NAME);
  }

  @Test
  void shouldSaveEventRequest() {
    service.processEventRequest(requestEvent);

    verify(returnRetrievalQueueRepository, times(1)).save(captor.capture());
    ReturnRetrievalQueueRecord record = captor.getValue();
    assertEquals(HOLD_ID, record.getHoldId());
    assertEquals(ITEM_BARCODE, record.getItemBarcode());
    assertEquals(CALL_NUMBER, record.getCallNumber());
    assertEquals(PATRON_BARCODE, record.getPatronBarcode());
    assertEquals(PATRON_NAME, record.getPatronName());
    assertEquals(PICKUP_SERVICE_POINT_CODE, record.getPickupLocation());
    assertEquals(STATUS, record.getRequestStatus());
    assertEquals(REQUEST_NOTE, record.getRequestNote());
    assertEquals(INSTANCE_TITLE, record.getInstanceTitle());
    assertEquals(INSTANCE_AUTHOR, record.getInstanceAuthor());
    assertEquals(REMOTE_STORAGE_ID, record.getRemoteStorageId().toString());
    assertNotNull(record.getCreatedDateTime());
    assertEquals(REQUEST_TYPE, record.getRequestType());
  }

  @Test()
  void shouldThrowExceptionWhenItemByBarcodeIsNotFound() {
    when(inventoryClient.getItemsByQuery("barcode==" + ITEM_BARCODE)).thenReturn(items);
    when(items.getResult()).thenReturn(Collections.EMPTY_LIST);

    assertThrows(EntityNotFoundException.class, () -> service.processEventRequest(requestEvent));
  }

  @Test
  void shouldNotProcessRecordsWhenLocationIsNotRemote() {
    when(locationMappingsService.getLocationMapping(EFFECTIVE_LOCATION_ID)).thenReturn(null);

    service.processEventRequest(requestEvent);

    verify(returnRetrievalQueueRepository, never()).save(isA(ReturnRetrievalQueueRecord.class));
  }

  @Test
  void shouldThrowExceptionWhenPatronIsNotFound() {
    when(usersClient.getUsersByQuery("id==" + REQUESTER_ID)).thenReturn(users);
    when(users.getResult()).thenReturn(Collections.EMPTY_LIST);

    assertThrows(EntityNotFoundException.class, () -> service.processEventRequest(requestEvent));
  }

  @Test
  void shouldNotSaveRequestWhenEffectiveLocationIsNotFound() {
    when(item.getEffectiveLocation()).thenReturn(null);

    verify(returnRetrievalQueueRepository, never()).save(isA(ReturnRetrievalQueueRecord.class));
  }
}
