package org.folio.rs.util;

import static org.apache.commons.lang3.StringUtils.EMPTY;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.IOUtils;

@Log4j2
public class JsonUtils {
  public static String readJson(String filename) {
    String value = EMPTY;
    try (InputStream inputStream = Utils.class
        .getClassLoader()
        .getResourceAsStream(filename)) {
      if (Objects.nonNull(inputStream)) {
        value = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
      }
    } catch (Exception e) {
      log.error("Exception during reading json file");
      value = EMPTY;
    }
    return value;
  }
}
