package org.folio.rs.util;

import org.apache.commons.lang3.StringUtils;

import java.util.UUID;

public class MapperUtils {
  private MapperUtils(){}

  public static UUID stringToUUIDSafe(String uuid) {
    return (StringUtils.isBlank(uuid)) ? null : UUID.fromString(uuid);
  }

  public static String uuidToStringSafe(UUID uuid) {
    return uuid != null ? uuid.toString() : null;
  }
}
