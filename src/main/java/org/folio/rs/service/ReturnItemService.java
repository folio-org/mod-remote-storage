package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemContributorNames;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.ReturnItemResponse;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.RetrievalQueueRecord;
import org.folio.rs.error.ItemReturnException;
import org.folio.rs.repository.RetrievalQueueRepository;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toList;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReturnItemService {

  private static final String BARCODE_QUERY_PROPERTY = "barcode==";
  private static final String USER_ID_QUERY_PROPERTY = "id==";

  private final InventoryClient inventoryClient;
  private final CirculationClient circulationClient;
  private final UsersClient usersClient;
  private final RetrievalQueueRepository retrievalQueueRepository;
  private final CheckInItemService checkInItemService;
  private final ServicePointsClient servicePointsClient;

  public ReturnItemResponse returnItem(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.info("Start return for item with barcode " + checkInItem.getItemBarcode());
    var itemReturnResponse = new ReturnItemResponse();
    var item = getItem(checkInItem);
    var requests = circulationClient.getItemRequests(item.getId());
    if (!requests.isEmpty()) {
      var holdRecallRequests = requests.getResult().stream()
        .filter(request -> request.getRequestType() == Request.RequestType.HOLD
          || request.getRequestType() == Request.RequestType.RECALL)
        .collect(toList());
      if (!holdRecallRequests.isEmpty()) {
        itemReturnResponse.isHoldRecallRequestExist(true);
        holdRecallRequests.stream()
          .filter(itemRequest -> itemRequest.getPosition() == 1)
          .findFirst().ifPresent(itemRequest-> {
            var user = getUser(itemRequest.getRequesterId());
            var servicePointCode = servicePointsClient.getServicePoint(itemRequest.getPickupServicePointId()).getCode();
            retrievalQueueRepository.save(buildRetrievalRecord(itemRequest, item, user, servicePointCode, remoteStorageConfigurationId));
        });
      }
    }
    checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    log.info("Return success for item with barcode " + checkInItem.getItemBarcode());
    return itemReturnResponse;
  }

  private Item getItem(CheckInItem checkInItem) {
    var items = inventoryClient.getItemsByQuery(BARCODE_QUERY_PROPERTY + checkInItem.getItemBarcode());
    if (items.isEmpty()) {
      throw new ItemReturnException("Item does not exist for barcode " + checkInItem.getItemBarcode());
    }
    return items.getResult().get(0);
  }

  private User getUser(String requesterId) {
    var users = usersClient.getUsersByQuery(USER_ID_QUERY_PROPERTY + requesterId);
    if (users.isEmpty()) {
      throw new ItemReturnException("User does not exist for requester id " + requesterId);
    }
    return users.getResult().get(0);
  }

  private RetrievalQueueRecord buildRetrievalRecord(Request itemRequest, Item item, User patron, String servicePointCode, String remoteStorageId) {
    return RetrievalQueueRecord.builder()
      .id(UUID.randomUUID())
      .holdId(itemRequest.getId())
      .itemBarcode(item.getBarcode())
      .instanceTitle(item.getTitle())
      .instanceAuthor(ofNullable(item
        .getContributorNames()).orElse(Collections.emptyList())
        .stream()
        .map(ItemContributorNames::getName)
        .collect(Collectors.joining(";")))
      .callNumber(ofNullable(item.getEffectiveCallNumberComponents())
        .map(ItemEffectiveCallNumberComponents::getCallNumber)
        .orElse(null))
      .patronBarcode(patron.getBarcode())
      .patronName(patron.getUsername())
      .pickupLocation(servicePointCode)
      .requestStatus(ofNullable(itemRequest.getStatus())
        .map(Request.Status::value).orElse(null))
      .requestNote(itemRequest.getPatronComments())
      .createdDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(remoteStorageId))
      .build();
  }
}
