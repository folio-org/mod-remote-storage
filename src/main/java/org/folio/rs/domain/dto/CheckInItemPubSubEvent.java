package org.folio.rs.domain.dto;

import org.folio.rs.mapper.deserializer.CheckInItemEventDeserializer;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

@JsonDeserialize(using = CheckInItemEventDeserializer.class)
public class CheckInItemPubSubEvent extends ItemCheckInPubSubEvent {
}
