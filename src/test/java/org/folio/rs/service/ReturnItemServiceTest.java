package org.folio.rs.service;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.ResultList;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.dto.ItemRequest;
import org.folio.rs.dto.Request;
import org.folio.rs.repository.RetrievalQueueRepository;
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

  @Mock
  private InventoryClient inventoryClient;
  @Mock
  private CirculationClient circulationClient;
  @Mock
  private RetrievalQueueRepository retrievalQueueRepository;
  @Mock
  private CheckInItemService checkInItemService;

  @InjectMocks
  private ReturnItemService returnItemService;

  @Test
  public void testReturnItemService() {
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode("item_barcode");

    var item = new Item("item_id", "item_barcode", null, null, null);
    var itemResult = new ResultList<Item>();
    itemResult.setTotalRecords(1);
    itemResult.setResult(Collections.singletonList(item));

    var request = new Request();
    request.setRequestType(Request.RequestType.HOLD);
    request.setPosition(1);
    request.setStatus(Request.Status.CLOSED_CANCELLED);
    ItemRequest itemRequests = new ItemRequest();
    itemRequests.setRequests(Collections.singletonList(request));
    itemRequests.setTotalRecords(1);

    when(inventoryClient.getItemsByQuery("barcode==" + item.getBarcode())).thenReturn(itemResult);
    when(circulationClient.getItemRequests(item.getId())).thenReturn(itemRequests);
    when(retrievalQueueRepository.save(isA(RetrievalQueueRecord.class))).thenReturn(null);
    doNothing().when(checkInItemService).checkInItemByBarcode(isA(String.class), isA(CheckInItem.class));

    var returnItemResponse = returnItemService.returnItem(REMOTE_STORAGE_CONFIGURATION_ID, checkInItem);

    verify(retrievalQueueRepository, times(1)).save(isA(RetrievalQueueRecord.class));
    assertThat(returnItemResponse.getIsHoldRecallRequestExist(), is(true));
  }
}
