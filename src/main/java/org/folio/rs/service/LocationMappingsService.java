package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.mapper.LocationMappingsMapper;
import org.folio.rs.repository.LocationMappingsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
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

  public LocationMapping getMappingByFolioLocationId(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    return locationMappingsRepository.findById(id).map(locationMappingsMapper::mapEntityToDto).orElse(null);
  }

  public LocationMappings getMappings(Integer offset, Integer limit) {
    var mappings = locationMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return locationMappingsMapper.mapEntitiesToMappingCollection(mappings);
  }

  public void deleteMappingById(String mappingId) {
    var id = UUID.fromString(mappingId);
    locationMappingsRepository.deleteById(id);
  }
}
