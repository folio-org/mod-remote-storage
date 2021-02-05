package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.domain.dto.CheckInByBarcodeRequest;
import org.folio.rs.domain.dto.ItemBarcode;
import org.joda.time.DateTime;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import static org.joda.time.DateTimeZone.UTC;

@Service
@RequiredArgsConstructor
@Log4j2
public class CheckInItemService {

  private final CirculationClient circulationClient;

  public HttpStatus checkInItemByBarcode(ItemBarcode itemBarcode) {
    log.info("Start check-in process for item with barcode " + itemBarcode.getValue());
    var responseEntity = circulationClient.checkIn(
      CheckInByBarcodeRequest.of(itemBarcode.getValue(), "026defb2-b953-4902-b143-3bdb6c2160fa", DateTime.now(UTC)));
    if (responseEntity.getStatusCode() != HttpStatus.OK) {
      log.error("Check-in call error for item with barcode " + itemBarcode.getValue());
      return HttpStatus.INTERNAL_SERVER_ERROR;
    }
    return HttpStatus.OK;
  }
}
