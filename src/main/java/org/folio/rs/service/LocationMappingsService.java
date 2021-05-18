package org.folio.rs.service;

import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.LocationMapping;
import org.folio.rs.domain.dto.LocationMappings;
import org.folio.rs.mapper.LocationMappingsMapper;
import org.folio.rs.repository.LocationMappingsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  public static final String MAPPINGS = "mappings";
  private final LocationMappingsRepository locationMappingsRepository;
  private final LocationMappingsMapper locationMappingsMapper;
  private final LocationClient locationClient;

  @CachePut(value = MAPPINGS, key = "#locationMapping.finalLocationId")
  public LocationMapping postMapping(LocationMapping locationMapping) {
    return locationMappingsMapper
      .mapEntityToDto(locationMappingsRepository.save(locationMappingsMapper.mapDtoToEntity(locationMapping)));
  }

  @Cacheable(value = MAPPINGS, key = "#finalLocationId")
  public LocationMapping getMappingByFinalLocationId(String finalLocationId) {
    var id = UUID.fromString(finalLocationId);
    return locationMappingsRepository.findById(id)
      .map(locationMappingsMapper::mapEntityToDto)
      .orElse(null);
  }

  public LocationMappings getMappings(Integer offset, Integer limit) {
    var mappings = locationMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return locationMappingsMapper.mapEntitiesToMappingCollection(mappings);
  }

  public LocationMappings getMappingsLocations(Integer offset, Integer limit) {
    var mappings = locationClient.getLocations().getResult().stream()
      .map(folioLocation -> {
        var locationMappings = locationMappingsRepository
          .findById(UUID.fromString(folioLocation.getId()));
        var locationMapping = new LocationMapping();
        locationMapping.setOriginalLocationId(folioLocation.getId());
        if (locationMappings.isPresent()) { // Case when mapping exists.
          locationMapping.setFinalLocationId(locationMappings.get().getFinalLocationId().toString());
        }
        return locationMappingsMapper.mapDtoToEntity(locationMapping);
      }).collect(Collectors.toList());
    return locationMappingsMapper.mapEntitiesToMappingCollection(
      new PageImpl<>(mappings, new OffsetRequest(offset, limit, Sort.unsorted()), mappings.size()));
  }

  @CacheEvict(value = MAPPINGS, key = "#finalLocationId")
  public void deleteMappingById(String finalLocationId) {
    var id = UUID.fromString(finalLocationId);
    locationMappingsRepository.deleteById(id);
  }
}
