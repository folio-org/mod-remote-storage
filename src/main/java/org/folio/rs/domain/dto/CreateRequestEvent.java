package org.folio.rs.domain.dto;

import org.folio.rs.mapper.CreateRequestEventDeserializer;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

@JsonDeserialize(using = CreateRequestEventDeserializer.class)
public class CreateRequestEvent extends EventRequest {
}
