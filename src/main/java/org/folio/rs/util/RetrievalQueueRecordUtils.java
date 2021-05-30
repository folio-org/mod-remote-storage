package org.folio.rs.util;

import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemContributorNames;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.PickupServicePoint;
import org.folio.rs.domain.dto.Request;
import org.folio.rs.domain.dto.RequestEvent;
import org.folio.rs.domain.dto.User;
import org.folio.rs.domain.entity.ReturnRetrievalQueueRecord;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

public class RetrievalQueueRecordUtils {

  private RetrievalQueueRecordUtils() {
  }

  public static ReturnRetrievalQueueRecord buildRetrievalRecord(Request itemRequest, RequestType requestType, Item item, User patron, String servicePointCode, String remoteStorageId) {
    return ReturnRetrievalQueueRecord.builder()

      .holdId(itemRequest.getId())
      .requestStatus(ofNullable(itemRequest.getStatus())
        .map(Request.Status::value).orElse(null))
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

  public static ReturnRetrievalQueueRecord buildRetrievalQueueRecord(RequestEvent requestEvent,
                                                               Item item, User patron, LocationMapping mapping, PickupServicePoint pickupServicePoint) {
    return ReturnRetrievalQueueRecord.builder()
      .id(UUID.randomUUID())
      .holdId(requestEvent.getHoldId())
      .itemBarcode(requestEvent.getItemBarCode())
      .requestStatus(requestEvent.getRequestStatus())
      .requestNote(requestEvent.getRequestNote())

      .patronBarcode(patron.getBarcode())
      .patronName(patron.getUsername())
      .callNumber(getCallNumber(item))

      .createdDateTime(LocalDateTime.now())
      .pickupLocation(pickupServicePoint.getCode())

      .remoteStorageId(stringToUUIDSafe(mapping.getConfigurationId()))
      .instanceTitle(item.getTitle())
      .instanceAuthor(getContributorNames(item))
      .requestType(RequestType.PYR.getType())
      .build();
  }

  private static String getContributorNames(Item item) {
    return isEmpty(item.getContributorNames())
      ? null
      : item.getContributorNames().stream()
      .map(ItemContributorNames::getName)
      .collect(Collectors.joining("; "));
  }

  private static String getCallNumber(Item item) {
    ItemEffectiveCallNumberComponents components = item.getEffectiveCallNumberComponents();
    return Objects.nonNull(components) ? components.getCallNumber() : null;
  }
}
