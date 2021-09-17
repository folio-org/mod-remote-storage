package org.folio.rs.service;

import static org.folio.rs.domain.dto.ReturningWorkflowDetails.CAIASOFT;
import static org.folio.rs.domain.dto.ReturningWorkflowDetails.FOLIO;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.error.ItemReturnException;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class ReturnItemServiceTest {

  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
  private static final String WRONG_REMOTE_STORAGE_CONFIGURATION_ID = "6db3fb62-0606-432c-a7ae-1204f98b82b7";
  private static final String ITEM_BARCODE = "item_barcode";
  private static final String ITEM_ID = "item_id";
  private static final String USER_ID = "user_id";
  @Mock
  private InventoryClient inventoryClient;
  @Mock
  private CirculationClient circulationClient;
  @Mock
  private UsersClient usersClient;
  @Mock
  private ReturnRetrievalQueueRepository returnRetrievalQueueRepository;
  @Mock
  private CheckInItemService checkInItemService;
  @Mock
  private ServicePointsClient servicePointsClient;
  @Mock
  private ConfigurationsService configurationsService;

  @InjectMocks
  private ReturnItemService returnItemService;

  @ParameterizedTest
  @MethodSource("getConfigurationsWithRequestsCheckNeeded")
  void shouldCreateRetrievalQueueRecordIfProviderIsNotCaiaSoftOrReturningFlowDetailsIsScannedToCaiaSoft(StorageConfiguration configuration) {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var user = new User();
    user.setId(USER_ID);
    var userResult = new ResultList<User>();
    userResult.setTotalRecords(1);
    userResult.setResult(Collections.singletonList(user));

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var request = new Request();
    request.setRequestType(Request.RequestType.HOLD);
    request.setPosition(1);
    request.setRequesterId(USER_ID);
    request.setStatus(Request.Status.CLOSED_CANCELLED);
    request.setPickupServicePointId("service-point-id");
    var itemRequests = new ResultList<Request>();
    itemRequests.setResult(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    var pickUpServicePoint = new PickupServicePoint();
    pickUpServicePoint.setId("service-point-id");
    pickUpServicePoint.setCode("service-point-code");

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(usersClient.getUsersByQuery("id==" + USER_ID)).thenReturn(userResult);
    when(returnRetrievalQueueRepository.save(isA(ReturnRetrievalQueueRecord.class))).thenReturn(null);
    when(servicePointsClient.getServicePoint(request.getPickupServicePointId())).thenReturn(pickUpServicePoint);
    when(configurationsService.getConfigurationById(isA(String.class))).thenReturn(configuration);
    doNothing().when(checkInItemService).checkInItemByBarcode(isA(String.class), isA(CheckInItem.class));

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(returnRetrievalQueueRepository, times(1)).save(isA(ReturnRetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(true));
  }

  @Test
  void shouldNotCreateRetrievalQueueRecordIfProviderIsCaiaSoftAndReturningFlowDetailsIsScannedToFolio() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var pickUpServicePoint = new PickupServicePoint();
    pickUpServicePoint.setId("service-point-id");
    pickUpServicePoint.setCode("service-point-code");

    var configuration = new StorageConfiguration()
      .name("test")
      .providerName("CAIA_SOFT")
      .returningWorkflowDetails(FOLIO);

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);
    when(configurationsService.getConfigurationById(isA(String.class))).thenReturn(configuration);
    doNothing().when(checkInItemService).checkInItemByBarcode(isA(String.class), isA(CheckInItem.class));

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(returnRetrievalQueueRepository, times(0)).save(isA(ReturnRetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(false));
  }

  @ParameterizedTest
  @MethodSource("getAllConfigurations")
  void testReturnItemIfRequestIsNotRecallOrHold(StorageConfiguration configuration) {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var request = new Request();
    request.setRequestType(Request.RequestType.PAGE);
    request.setPosition(1);
    request.setStatus(Request.Status.CLOSED_CANCELLED);
    request.setRequesterId(USER_ID);
    var itemRequests = new ResultList<Request>();
    itemRequests.setResult(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);
    lenient().when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(configurationsService.getConfigurationById(isA(String.class))).thenReturn(configuration);

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(returnRetrievalQueueRepository, times(0)).save(isA(ReturnRetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(false));
  }

  @ParameterizedTest
  @MethodSource("getAllConfigurations")
  void testReturnItemIfItemRequestsNotExist(StorageConfiguration configuration) {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var itemRequests = new ResultList<Request>();
    itemRequests.setResult(Collections.emptyList());
    itemRequests.setTotalRecords(0);

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);
    lenient().when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(configurationsService.getConfigurationById(isA(String.class))).thenReturn(configuration);

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(returnRetrievalQueueRepository, times(0)).save(isA(ReturnRetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(false));
  }

  @Test
  void testReturnItemIfItemByBarcodeNotExist() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(0);
    itemResult.setResult(Collections.emptyList());

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);

    Assertions.assertThrows(ItemReturnException.class,
      () -> returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }

  @Test
  void shouldThrowItemReturnExceptionOnWrongStorageConfigurationId() {
    var checkInItem = new CheckInItem().itemBarcode(ITEM_BARCODE);

    var item = new Item().id(ITEM_ID).barcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(0);
    itemResult.setResult(Collections.emptyList());

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);

    Assertions.assertThrows(ItemReturnException.class,
      () -> returnItemService.returnItem(WRONG_REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }

  @Test
  void testReturnItemIfUserByByRequestIdNotExist() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var userResult = new ResultList<User>();
    userResult.setTotalRecords(0);
    userResult.setResult(Collections.emptyList());

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var request = new Request();
    request.setRequestType(Request.RequestType.HOLD);
    request.setPosition(1);
    request.setRequesterId(USER_ID);
    request.setStatus(Request.Status.CLOSED_CANCELLED);
    var itemRequests = new ResultList<Request>();
    itemRequests.setResult(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    var configuration = new StorageConfiguration()
      .name("test")
      .providerName("DEMATIC_EMS");

    when(inventoryClient.getItemByBarcode(item.getBarcode())).thenReturn(item);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(usersClient.getUsersByQuery("id==" + USER_ID)).thenReturn(userResult);
    when(configurationsService.getConfigurationById(isA(String.class))).thenReturn(configuration);

    Assertions.assertThrows(ItemReturnException.class,
      () -> returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }

  private static List<StorageConfiguration> getConfigurationsWithRequestsCheckNeeded() {
    return Arrays.asList(
      new StorageConfiguration().name("test").providerName("DEMATIC_EMS"),
      new StorageConfiguration().name("test").providerName("DEMATIC_SD"),
      new StorageConfiguration().name("test").providerName("CAIA_SOFT").returningWorkflowDetails(CAIASOFT)
    );
  }

  private static List<StorageConfiguration> getAllConfigurations() {
    var configurations = new ArrayList<>(getConfigurationsWithRequestsCheckNeeded());
    configurations.add(new StorageConfiguration().name("test").providerName("CAIA_SOFT").returningWorkflowDetails(FOLIO));
    return configurations;
  }
}
