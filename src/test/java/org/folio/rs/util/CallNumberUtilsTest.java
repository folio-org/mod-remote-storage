package org.folio.rs.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.folio.rs.domain.dto.Item;
import org.folio.rs.domain.dto.ItemEffectiveCallNumberComponents;
import org.junit.jupiter.api.Test;

class CallNumberUtilsTest {

  @Test
  void shouldReturnNullWhenItemIsNull() {
    assertNull(CallNumberUtils.buildEffectiveCallNumber(null));
  }

  @Test
  void shouldReturnNullWhenNoCallNumberDataAvailable() {
    assertNull(CallNumberUtils.buildEffectiveCallNumber(new Item()));
  }

  @Test
  void shouldReturnEffectiveCallNumberOnlyWhenNoOtherComponents() {
    var item = new Item().effectiveCallNumberComponents(
      new ItemEffectiveCallNumberComponents().callNumber("K1 .M44"));

    assertEquals("K1 .M44", CallNumberUtils.buildEffectiveCallNumber(item));
  }

  @Test
  void shouldCombineAllComponentsInCanonicalOrder() {
    var item = new Item()
      .effectiveCallNumberComponents(new ItemEffectiveCallNumberComponents()
        .prefix("PRE")
        .callNumber("K1 .M44")
        .suffix("SUF"))
      .displaySummary("summary")
      .volume("vol.3")
      .enumeration("v.71:no.6-2")
      .chronology("1985:July-Dec.")
      .copyNumber("c.2");

    assertEquals("PRE K1 .M44 SUF summary vol.3 v.71:no.6-2 1985:July-Dec. c.2",
      CallNumberUtils.buildEffectiveCallNumber(item));
  }

  @Test
  void shouldSkipBlankComponents() {
    var item = new Item()
      .effectiveCallNumberComponents(new ItemEffectiveCallNumberComponents()
        .prefix(" ")
        .callNumber("K1 .M44")
        .suffix(null))
      .displaySummary(null)
      .volume(null)
      .enumeration("v.71:no.6-2")
      .chronology("")
      .copyNumber("c.4");

    assertEquals("K1 .M44 v.71:no.6-2 c.4", CallNumberUtils.buildEffectiveCallNumber(item));
  }

  @Test
  void shouldBuildCallNumberFromEnumerationWhenNoEffectiveComponents() {
    var item = new Item().enumeration("v.71:no.6-2");

    assertEquals("v.71:no.6-2", CallNumberUtils.buildEffectiveCallNumber(item));
  }
}
