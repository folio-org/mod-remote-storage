package org.folio.rs.controller;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;

import org.folio.rs.domain.dto.AccessionFilterData;
import org.folio.rs.domain.dto.RetrievalQueues;
import org.folio.rs.rest.resource.RetrievalsApi;
import org.folio.rs.service.ReturnRetrievalQueueService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage")
public class RetrieveController implements RetrievalsApi {

  private final ReturnRetrievalQueueService returnRetrievalQueueService;

  @Override
  public ResponseEntity<RetrievalQueues> getRetrievals(@Valid Boolean retrieved, @Valid String storageId,
      @Valid String createdDateTime, @Min(0) @Max(2147483647) @Valid Integer offset,
      @Min(0) @Max(2147483647) @Valid Integer limit) {
    var retrievalQueueRecords = returnRetrievalQueueService
      .getRetrievals(getFilterData(retrieved, storageId, createdDateTime, offset, limit));
    return new ResponseEntity<>(retrievalQueueRecords, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> setRetrievedById(
      @Pattern(regexp = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$") String retrievalId) {
    returnRetrievalQueueService.setRetrievedById(retrievalId);
    return ResponseEntity.noContent()
      .build();
  }

  @Override
  public ResponseEntity<String> setRetrievedByBarcode(String barcode) {
    returnRetrievalQueueService.setRetrievedByBarcode(barcode);
    return ResponseEntity.noContent()
      .build();
  }

  private AccessionFilterData getFilterData(Boolean retrieved, String storageId, String createdDate, Integer offset, Integer limit) {
    return AccessionFilterData.builder()
      .isPresented(retrieved)
      .storageId(storageId)
      .createDate(createdDate)
      .offset(offset)
      .limit(limit)
      .build();
  }
}
