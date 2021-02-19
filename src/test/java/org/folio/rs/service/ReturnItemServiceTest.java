package org.folio.rs.service;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.ItemRequests;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.error.ItemReturnException;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ReturnItemServiceTest {

  private static final String REMOTE_STORAGE_CONFIGURATION_ID = "de17bad7-2a30-4f1c-bee5-f653ded15629";
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
  private RetrievalQueueRepository retrievalQueueRepository;
  @Mock
  private CheckInItemService checkInItemService;

  @InjectMocks
  private ReturnItemService returnItemService;

  @Test
  void testReturnItem() {
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
    ItemRequests itemRequests = new ItemRequests();
    itemRequests.setRequests(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(usersClient.getUsersByQuery("id==" + USER_ID)).thenReturn(userResult);
    when(retrievalQueueRepository.save(isA(RetrievalQueueRecord.class))).thenReturn(null);
    doNothing().when(checkInItemService).checkInItemByBarcode(isA(String.class), isA(CheckInItem.class));

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(retrievalQueueRepository, times(1)).save(isA(RetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(true));
  }

  @Test
  void testReturnItemIfRequestIsNotRecallOrHold() {
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
    ItemRequests itemRequests = new ItemRequests();
    itemRequests.setRequests(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(retrievalQueueRepository, times(0)).save(isA(RetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(false));
  }

  @Test
  void testReturnItemIfItemRequestsNotExist() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(ITEM_BARCODE);

    var item = new Item();
    item.setId(ITEM_ID);
    item.setBarcode(ITEM_BARCODE);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    ItemRequests itemRequests = new ItemRequests();
    itemRequests.setRequests(Collections.emptyList());
    itemRequests.setTotalRecords(0);

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(retrievalQueueRepository, times(0)).save(isA(RetrievalQueueRecord.class));
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

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);

    Assertions.assertThrows(ItemReturnException.class,
      () -> returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
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
    ItemRequests itemRequests = new ItemRequests();
    itemRequests.setRequests(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(usersClient.getUsersByQuery("id==" + USER_ID)).thenReturn(userResult);

    Assertions.assertThrows(ItemReturnException.class,
      () -> returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem));
  }
}
