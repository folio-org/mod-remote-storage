package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.mapper.LocationMappingsMapper;
import org.folio.rs.repository.LocationMappingsRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  private final LocationMappingsRepository locationMappingsRepository;
  private final LocationMappingsMapper locationMappingsMapper;

  public LocationMapping postMapping(LocationMapping locationMapping) {
    return locationMappingsMapper
      .mapEntityToDto(locationMappingsRepository.save(locationMappingsMapper.mapDtoToEntity(locationMapping)));
  }

  public void deleteMappingById(String mappingId) {
    var id = UUID.fromString(mappingId);

    locationMappingsRepository.deleteById(id);
  }
}
