package org.folio.rs.service;

import static java.util.Objects.isNull;
import static org.folio.rs.domain.dto.Request.RequestType.HOLD;
import static org.folio.rs.domain.dto.Request.RequestType.RECALL;
import static org.folio.rs.domain.dto.ReturningWorkflowDetails.CAIASOFT;
import static org.folio.rs.domain.entity.ProviderRecord.CAIA_SOFT;
import static org.folio.rs.util.RetrievalQueueRecordUtils.buildReturnRetrievalRecord;

import java.util.Optional;

import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ServicePointsClient;
import org.folio.rs.client.UsersClient;
import org.folio.rs.domain.dto.CheckInItem;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemCheckInPubSubEvent;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.ReturnItemResponse;
import org.folio.rs.domain.dto.ReturningWorkflowDetails;
import org.folio.rs.domain.dto.StorageConfiguration;
import org.folio.rs.domain.dto.User;
import org.folio.rs.error.ItemReturnException;
import org.folio.rs.repository.ReturnRetrievalQueueRepository;
import org.folio.rs.util.RequestType;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReturnItemService {

  private static final String BARCODE_QUERY_PROPERTY = "barcode==";
  private static final String USER_ID_QUERY_PROPERTY = "id==";

  private final InventoryClient inventoryClient;
  private final CirculationClient circulationClient;
  private final UsersClient usersClient;
  private final ReturnRetrievalQueueRepository returnRetrievalQueueRepository;
  private final CheckInItemService checkInItemService;
  private final ServicePointsClient servicePointsClient;
  private final ConfigurationsService configurationsService;
  private final LocationMappingsService locationMappingsService;

  public ReturnItemResponse returnItem(String remoteStorageConfigurationId, CheckInItem checkInItem) {
    log.info("Start return for item with barcode " + checkInItem.getItemBarcode());
    var itemReturnResponse = new ReturnItemResponse();
    var item = getItemByBarcode(checkInItem.getItemBarcode());
    var storageConfiguration = getStorageConfigurationById(remoteStorageConfigurationId);
    if (isRequestsCheckNeeded(storageConfiguration)) {
      findFirstHoldRecallRequest(item).ifPresent(request -> {
        itemReturnResponse.isHoldRecallRequestExist(true);
        var user = getUserById(request.getRequesterId());
        var servicePointCode = servicePointsClient.getServicePoint(request.getPickupServicePointId()).getCode();
        returnRetrievalQueueRepository.save(buildReturnRetrievalRecord(request, RequestType.PYR, item, user, servicePointCode, remoteStorageConfigurationId));
      });
    }
    checkInItemService.checkInItemByBarcode(remoteStorageConfigurationId, checkInItem);
    log.info("Return success for item with barcode " + checkInItem.getItemBarcode());
    return itemReturnResponse;
  }

  public ReturnItemResponse returnItem(String itemBarcode) {
    log.info("Start return for item with barcode " + itemBarcode);

    var itemReturnResponse = new ReturnItemResponse();
    var item = getItemByBarcode(itemBarcode);
    var locationMapping = getLocationMapping(item.getEffectiveLocation().getId());
    var storageConfiguration = getStorageConfigurationById(locationMapping.getConfigurationId());

    if (CAIA_SOFT.getId().equals(storageConfiguration.getProviderName())
      && storageConfiguration.getReturningWorkflowDetails() == ReturningWorkflowDetails.FOLIO) {
      findFirstHoldRecallRequest(item).ifPresent(request -> {
        itemReturnResponse.isHoldRecallRequestExist(true);
        var user = getUserById(request.getRequesterId());
        var servicePointCode = servicePointsClient.getServicePoint(request.getPickupServicePointId()).getCode();
        returnRetrievalQueueRepository.save(buildReturnRetrievalRecord(request, RequestType.REF, item, user, servicePointCode, locationMapping.getConfigurationId()));
      });
    }
    var checkInItem = new CheckInItem();
    checkInItem.setItemBarcode(itemBarcode);
    checkInItemService.checkInItemByBarcode(locationMapping.getConfigurationId(), checkInItem);
    return itemReturnResponse;
  }

  private StorageConfiguration getStorageConfigurationById(String remoteStorageConfigurationId) {
    var configuration = configurationsService.getConfigurationById(remoteStorageConfigurationId);
    if (isNull(configuration)) {
      throw new ItemReturnException("Remote storage configuration does not exist for id " + remoteStorageConfigurationId);
    }
    return configuration;
  }

  private RemoteLocationConfigurationMapping getLocationMapping(String originalLocationId) {
    var locationMapping = locationMappingsService.getRemoteLocationConfigurationMapping(originalLocationId);
    if (isNull(locationMapping)) {
      throw new ItemReturnException("Mapping does not exist for folioLocationId " + originalLocationId);
    }
    return locationMapping;
  }

  private boolean isRequestsCheckNeeded(StorageConfiguration storageConfiguration) {
    return !CAIA_SOFT.getId().equals(storageConfiguration.getProviderName()) ||
      CAIASOFT == storageConfiguration.getReturningWorkflowDetails();
  }

  private Optional<Request> findFirstHoldRecallRequest(Item item) {
    var requests = circulationClient.getItemRequests(item.getId());
    if (!requests.isEmpty()) {
      return requests.getResult().stream()
        .filter(request -> request.getPosition() == 1 &&
          (request.getRequestType() == HOLD || request.getRequestType() == RECALL))
        .findFirst();
    }
    return Optional.empty();
  }

  private Item getItemByBarcode(String barcode) {
    var items = inventoryClient.getItemsByQuery(BARCODE_QUERY_PROPERTY + barcode);
    if (items.isEmpty()) {
      throw new ItemReturnException("Item does not exist for barcode " + barcode);
    }
    return items.getResult().get(0);
  }

  private User getUserById(String requesterId) {
    var users = usersClient.getUsersByQuery(USER_ID_QUERY_PROPERTY + requesterId);
    if (users.isEmpty()) {
      throw new ItemReturnException("User does not exist for requester id " + requesterId);
    }
    return users.getResult().get(0);
  }
}
