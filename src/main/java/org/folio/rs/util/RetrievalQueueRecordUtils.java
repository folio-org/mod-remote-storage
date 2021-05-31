package org.folio.rs.util;

import static java.util.Optional.ofNullable;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemContributorNames;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;

public class RetrievalQueueRecordUtils {

  private RetrievalQueueRecordUtils() {
  }

  public static ReturnRetrievalQueueRecord buildReturnRetrievalRecord(Request itemRequest, RequestType requestType, Item item,
      User patron, String servicePointCode, String remoteStorageId) {
    return ReturnRetrievalQueueRecord.builder()

      .holdId(itemRequest.getId())
      .requestStatus(ofNullable(itemRequest.getStatus()).map(Request.Status::value)
        .orElse(null))
      .requestNote(itemRequest.getPatronComments())

      .id(UUID.randomUUID())
      .itemBarcode(item.getBarcode())
      .instanceTitle(item.getTitle())
      .instanceAuthor(getContributorNames(item))
      .callNumber(getCallNumber(item))
      .patronBarcode(patron.getBarcode())
      .patronName(patron.getUsername())
      .pickupLocation(servicePointCode)

      .createdDateTime(LocalDateTime.now())
      .remoteStorageId(UUID.fromString(remoteStorageId))
      .requestType(requestType.getType())
      .build();
  }

  public static ReturnRetrievalQueueRecord buildReturnRetrievalQueueRecord(RequestEvent requestEvent, Item item, User patron,
      RemoteLocationConfigurationMapping mapping, PickupServicePoint pickupServicePoint) {
    return ReturnRetrievalQueueRecord.builder()
      .holdId(requestEvent.getHoldId())
      .requestStatus(requestEvent.getRequestStatus())
      .requestNote(requestEvent.getRequestNote())

      .id(UUID.randomUUID())
      .itemBarcode(requestEvent.getItemBarCode())
      .instanceTitle(item.getTitle())
      .instanceAuthor(getContributorNames(item))

      .callNumber(getCallNumber(item))
      .patronBarcode(patron.getBarcode())
      .patronName(patron.getUsername())
      .pickupLocation(pickupServicePoint.getCode())
      .createdDateTime(LocalDateTime.now())
      .remoteStorageId(stringToUUIDSafe(mapping.getConfigurationId()))
      .requestType(RequestType.PYR.getType())
      .build();
  }

  private static String getContributorNames(Item item) {
    return isEmpty(item.getContributorNames()) ? null
        : item.getContributorNames()
          .stream()
          .map(ItemContributorNames::getName)
          .collect(Collectors.joining("; "));
  }

  private static String getCallNumber(Item item) {
    ItemEffectiveCallNumberComponents components = item.getEffectiveCallNumberComponents();
    return Objects.nonNull(components) ? components.getCallNumber() : null;
  }
}
