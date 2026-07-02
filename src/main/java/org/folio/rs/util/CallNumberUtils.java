package org.folio.rs.util;

import static org.apache.commons.lang3.StringUtils.SPACE;

import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.folio.rs.domain.dto.Item;

public final class CallNumberUtils {

  private CallNumberUtils() {
  }

  /**
   * Builds the effective call number for an item.
   *
   * <p>
   * The value is composed, in order, of the effective call number prefix,
   * effective call number,
   * effective call number suffix, display summary, volume, enumeration,
   * chronology and copy number,
   * joined by a single space with blank components skipped. This mirrors the
   * canonical FOLIO
   * effective call number defined in stripes-util (lib/effectiveCallNumber.js) so
   * that staff can
   * look up a specific volume, year or issue of a serial/periodical directly in
   * the ASRS software.
   * </p>
   *
   * @param item {@link Item} entity
   * @return the composed effective call number, or {@code null} when no data is
   *         available
   */
  public static String buildEffectiveCallNumber(Item item) {
    if (Objects.isNull(item)) {
      return null;
    }
    var components = item.getEffectiveCallNumberComponents();
    var effectiveCallNumber = Stream.of(
        Objects.isNull(components) ? null : components.getPrefix(),
        Objects.isNull(components) ? null : components.getCallNumber(),
        Objects.isNull(components) ? null : components.getSuffix(),
        item.getDisplaySummary(),
        item.getVolume(),
        item.getEnumeration(),
        item.getChronology(),
        item.getCopyNumber())
        .filter(StringUtils::isNotBlank)
        .collect(Collectors.joining(SPACE));
    return effectiveCallNumber.isEmpty() ? null : effectiveCallNumber;
  }
}
