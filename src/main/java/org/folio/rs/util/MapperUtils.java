package org.folio.rs.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;

import java.util.UUID;
import org.folio.rs.domain.dto.MovedEventRequest;

@Log4j2
public class MapperUtils {
  private MapperUtils(){}

  public static UUID stringToUUIDSafe(String uuid) {
    return (StringUtils.isBlank(uuid)) ? null : UUID.fromString(uuid);
  }

  public static String uuidToStringSafe(UUID uuid) {
    return uuid != null ? uuid.toString() : null;
  }

  public static MovedEventRequest mapJsonToMovedEventRequest(String event) {
    try {
      return new ObjectMapper()
          .readerFor(MovedEventRequest.class)
          .readValue(event);
    } catch (JsonProcessingException exception) {
      log.error("Exception during moved event json mapping " + exception.getMessage());
      return null;
    }
  }
}
