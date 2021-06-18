package org.folio.rs.controller;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;

import org.folio.rs.domain.dto.AccessionQueue;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.AccessionRequest;
import org.folio.rs.domain.dto.AccessionFilterData;
import org.folio.rs.rest.resource.AccessionsApi;
import org.folio.rs.service.AccessionQueueService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage")
public class AccessionController implements AccessionsApi {

  private final AccessionQueueService accessionQueueService;

  @Override
  public ResponseEntity<AccessionQueues> getAccessions(@Valid Boolean accessioned, @Valid String storageId,
      @Valid String createdDate, @Min(0) @Max(2147483647) @Valid Integer offset, @Min(0) @Max(2147483647) @Valid Integer limit) {
    var accessionQueues = accessionQueueService.getAccessions(getFilterData(accessioned, storageId, createdDate, offset, limit));
    return new ResponseEntity<>(accessionQueues, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<AccessionQueue> postAccession(@RequestBody AccessionRequest accessionRequest) {
    return new ResponseEntity<>(accessionQueueService.processPostAccession(accessionRequest), HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<String> setAccessionedById(
      @Pattern(regexp = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$") String accessionId) {
    accessionQueueService.setAccessionedById(accessionId);
    return ResponseEntity.noContent()
      .build();
  }

  @Override
  public ResponseEntity<String> setAccessionedByBarcode(String barcode) {
    accessionQueueService.setAccessionedByBarcode(barcode);
    return ResponseEntity.noContent()
      .build();
  }

  private AccessionFilterData getFilterData(Boolean accessioned, String storageId, String createdDate, Integer offset, Integer limit) {
    return AccessionFilterData.builder()
      .isPresented(accessioned)
      .remoteStorageConfigurationId(storageId)
      .createDate(createdDate)
      .offset(offset)
      .limit(limit)
      .build();
  }
}
