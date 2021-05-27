package org.folio.rs.service;

import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.FullMapping;
import org.folio.rs.domain.dto.FullMappings;
import org.folio.rs.domain.dto.PlainMapping;
import org.folio.rs.domain.dto.PlainMappings;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.mapper.PlainMappingsMapper;
import org.folio.rs.mapper.FullMappingsMapper;
import org.folio.rs.repository.FullMappingsRepository;
import org.folio.rs.repository.OriginalLocationsRepository;
import org.folio.rs.repository.PlainMappingsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  public static final String MAPPINGS = "mappings";
  private final PlainMappingsRepository plainMappingsRepository;
  private final PlainMappingsMapper plainMappingsMapper;
  private final LocationClient locationClient;
  private final OriginalLocationsRepository originalLocationsRepository;
  private final FullMappingsRepository fullMappingsRepository;

  public PlainMapping postPlainMapping(PlainMapping plainMapping) {
    return plainMappingsMapper
      .mapEntityToDto(plainMappingsRepository.save(plainMappingsMapper.mapDtoToEntity(plainMapping)));
  }

  public PlainMapping getPlainMapping(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    return plainMappingsRepository.findById(id)
      .map(plainMappingsMapper::mapEntityToDto)
      .orElse(null);
  }

  public PlainMappings getPlainMappings(Integer offset, Integer limit) {
    var mappings = plainMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return plainMappingsMapper.mapEntitiesToPlainMappingCollection(mappings);
  }

  public void deleteMappingById(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    plainMappingsRepository.deleteById(id);
  }

  public FullMapping postFullMapping(FullMapping fullMapping) {
    var entity = fullMappingsRepository.findById(UUID.fromString(fullMapping.getFinalLocationId()))
      .map(m -> {
        var locations = m.getOriginalLocations();
        var originalLocation = new OriginalLocation();
        originalLocation.setFolioLocationId(UUID.fromString(fullMapping.getFinalLocationId()));
        originalLocation.setOriginalLocationId(UUID.fromString(fullMapping.getOriginalLocationId()));
        locations.add(originalLocation);
        m.setOriginalLocations(locations);
        return fullMappingsRepository.save(m);
      })
      .orElseGet(() -> fullMappingsRepository.save(FullMappingsMapper.mapDtoToEntity(fullMapping)));
    return new FullMapping()
      .finalLocationId(entity.getFinalLocationId().toString())
      .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
      .originalLocationId(fullMapping.getOriginalLocationId());
  }

  public FullMappings getFullMappings(Integer offset, Integer limit) {
    var mappings = fullMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return FullMappingsMapper.mapEntitiesToDtos(mappings);
  }

  public FullMappings getFullMappings(String finalLocationId) {
    return fullMappingsRepository.findById(UUID.fromString(finalLocationId))
      .map(FullMappingsMapper::mapEntityToDto)
      .orElse(null);
  }

  public FullMappings getFullMappingsLocations() {
    var mappings = locationClient.getLocations().getResult().stream()
      .map(folioLocation -> {
        var mapping = originalLocationsRepository
          .findByOriginalLocationId(UUID.fromString(folioLocation.getId()));
        var locationMapping = new FullMapping();
        locationMapping.setOriginalLocationId(folioLocation.getId());
        mapping.ifPresent(m -> locationMapping.setFinalLocationId(m.getFolioLocationId().toString()));
        return locationMapping;
      }).collect(Collectors.toList());
    return new FullMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }
}
