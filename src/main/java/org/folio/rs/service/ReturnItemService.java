package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.ContributorName;
import org.folio.rs.domain.dto.EffectiveCallNumberComponents;
import org.folio.rs.domain.dto.Item;
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

  public ReturnItemResponse returnItem(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.info("Start return for item with barcode " + checkInItem.getItemBarcode());
    var itemReturnResponse = new ReturnItemResponse();
    var items = inventoryClient.getItemsByQuery(BARCODE_QUERY_PROPERTY + checkInItem.getItemBarcode());
    if (items.getResult().isEmpty()) {
      throw new ItemReturnException("Item does not exist for barcode " + checkInItem.getItemBarcode());
    }
    var item = items.getResult().get(0);
    var requests = circulationClient.getItemRequests(item.getId());
    if (!requests.getResult().isEmpty()) {
      var holdRecallRequests = requests.getResult().stream()
        .filter(request -> request.getRequestType() == Request.RequestType.HOLD
        || request.getRequestType() == Request.RequestType.RECALL)
        .collect(toList());
      if (!holdRecallRequests.isEmpty()) {
        itemReturnResponse.isHoldRecallRequestExist(true);
        var firstPositionRequest = holdRecallRequests.stream()
          .filter(request -> request.getPosition() == 1)
          .findFirst();
        if (firstPositionRequest.isPresent()) {
          var request = firstPositionRequest.get();
          var users = usersClient.getUsersByQuery(USER_ID_QUERY_PROPERTY + request.getRequesterId());
          if (users.getResult().isEmpty()) {
            throw new ItemReturnException("User does not exist for requester id " + request.getRequesterId());
          }
          var user = users.getResult().get(0);
          var retrievalQueueRecord = getRetrievalRecord(firstPositionRequest.get(), item, user, remoteStorageConfigurationId);
          retrievalQueueRepository.save(retrievalQueueRecord);
        }
      }
    }
    checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    log.info("Return success for item with barcode " + checkInItem.getItemBarcode());
    return itemReturnResponse;
  }

  private RetrievalQueueRecord getRetrievalRecord(Request request, Item item, User patron, String remoteStorageId) {
    return RetrievalQueueRecord.builder()
      .id(UUID.randomUUID())
      .holdId(item.getHoldingsRecordId())
      .itemBarcode(item.getBarcode())
      .instanceTitle(item.getTitle())
      .instanceAuthor(ofNullable(item
        .getContributorNames()).orElse(Collections.emptyList())
        .stream()
        .map(ContributorName::getName)
        .collect(Collectors.joining(";")))
      .callNumber(ofNullable(item.getEffectiveCallNumberComponents())
        .map(EffectiveCallNumberComponents::getCallNumber)
        .orElse(null))
      .patronBarcode(patron.getBarcode())
      .patronName(patron.getUsername())
      .pickupLocation(request.getPickupServicePointId())
      .requestStatus(ofNullable(request.getStatus())
        .map(Request.Status::value).orElse(null))
      .requestNote(request.getPatronComments())
      .createdDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(remoteStorageId))
      .build();
  }
}
