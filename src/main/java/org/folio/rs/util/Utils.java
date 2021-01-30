package org.folio.rs.util;

import java.util.UUID;

public class Utils {

  private Utils() {
  }

  public static String randomIdAsString() {
    return UUID.randomUUID().toString();
  }
}
