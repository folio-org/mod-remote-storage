package org.folio.rs.controller;

import java.time.format.DateTimeParseException;
import javax.persistence.EntityNotFoundException;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.AccessionQueues;
import org.folio.rs.domain.dto.FilterData;
import org.folio.rs.rest.resource.AccessionApi;
import org.folio.rs.service.AccessionQueueService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Log4j2
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/remote-storage")
public class AccessionController implements AccessionApi {
  private static final String ACCESSION_QUEUE_NOT_FOUND = "Accession queue not found";
  private static final String WRONG_DATE_FORMAT_MESSAGE = "Wrong date format for accession queue";

  private final AccessionQueueService accessionQueueService;

  @Override
  public ResponseEntity<AccessionQueues> getAccessions(@Valid Boolean accessioned, @Valid String storageId, @Valid String createdDate,
      @Min(0) @Max(2147483647) @Valid Integer offset, @Min(0) @Max(2147483647) @Valid Integer limit) {
    var accessionQueues = accessionQueueService.getAccessions(getFilterData(accessioned, storageId, createdDate, offset, limit));
    return new ResponseEntity<>(accessionQueues, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> setAccessioned(
      @Pattern(regexp = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[1-5][a-fA-F0-9]{3}-[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$") String accessionId) {
    accessionQueueService.setAccessioned(accessionId);
    return ResponseEntity.noContent().build();
  }

  @ExceptionHandler({EntityNotFoundException.class})
  public ResponseEntity<String> handleNotFoundExceptions() {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(ACCESSION_QUEUE_NOT_FOUND);
  }

  @ExceptionHandler({DateTimeParseException.class})
  public ResponseEntity<String> handleDateTimeFormatExceptions() {
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(WRONG_DATE_FORMAT_MESSAGE);
  }

  private FilterData getFilterData(Boolean accessioned, String storageId, String createdDate, Integer offset, Integer limit) {
    return FilterData.builder()
        .accessioned(accessioned)
        .storageId(storageId)
        .createDate(createdDate)
        .offset(offset)
        .limit(limit)
        .build();
  }
}
