package org.folio.rs.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.mapper.LocationMappingsMapper;
import org.folio.rs.repository.LocationMappingsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  public static final String MAPPINGS = "mappings";
  private final LocationMappingsRepository locationMappingsRepository;
  private final LocationMappingsMapper locationMappingsMapper;

  @CachePut(value = MAPPINGS, key = "#locationMapping.folioLocationId")
  public LocationMapping postMapping(LocationMapping locationMapping) {
    return locationMappingsMapper
      .mapEntityToDto(locationMappingsRepository.save(locationMappingsMapper.mapDtoToEntity(locationMapping)));
  }

  @Cacheable(value = MAPPINGS, key = "#folioLocationId")
  public LocationMapping getMappingByFolioLocationId(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    return locationMappingsRepository.findById(id).map(locationMappingsMapper::mapEntityToDto).orElse(null);
  }

  public LocationMappings getMappings(Integer offset, Integer limit) {
    var mappings = locationMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return locationMappingsMapper.mapEntitiesToMappingCollection(mappings);
  }

  public LocationMapping getLocationMapping(String originalLocationId, String remoteConfigurationId) {
    var mapping = new LocationMapping();
    mapping.setConfigurationId("b3354743-285d-468d-9fa1-4e3d6321c13d");
    return mapping;
  }

  @CacheEvict(value = MAPPINGS, key = "#folioLocationId")
  public void deleteMappingById(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    locationMappingsRepository.deleteById(id);
  }
}
