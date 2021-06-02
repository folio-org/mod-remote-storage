package org.folio.rs.domain.dto;

import org.folio.rs.mapper.deserializer.ChangeEventDeserializer;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

@JsonDeserialize(using = ChangeEventDeserializer.class)
public class ChangeRequestEvent extends RequestEvent {
}
