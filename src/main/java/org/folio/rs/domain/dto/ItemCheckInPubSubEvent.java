package org.folio.rs.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.With;

import java.util.UUID;

@Data
@With
@AllArgsConstructor
@NoArgsConstructor
public class ItemCheckInPubSubEvent {
  private String itemBarcode;
}
