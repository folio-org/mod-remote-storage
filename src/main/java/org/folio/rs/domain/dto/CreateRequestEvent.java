package org.folio.rs.domain.dto;

import org.folio.rs.mapper.deserializer.CreateEventDeserializer;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

@JsonDeserialize(using = CreateEventDeserializer.class)
public class CreateRequestEvent extends RequestEvent {
}
