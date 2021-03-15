package org.folio.rs.domain.dto;

import org.folio.rs.mapper.ChangeRequestEventDeserializer;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

@JsonDeserialize(using = ChangeRequestEventDeserializer.class)
public class ChangeRequestEvent extends EventRequest {
}
