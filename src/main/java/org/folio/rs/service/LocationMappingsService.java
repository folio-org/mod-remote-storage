package org.folio.rs.service;

import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper;
import org.folio.rs.mapper.ExtendedRemoteLocationConfigurationMappingsMapper;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.folio.rs.repository.OriginalLocationsRepository;
import org.folio.rs.repository.RemoteLocationConfigurationMappingsRepository;
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
  private final RemoteLocationConfigurationMappingsRepository mappingsRepository;
  private final RemoteLocationConfigurationMappingsMapper mappingsMapper;
  private final LocationClient locationClient;
  private final OriginalLocationsRepository originalLocationsRepository;
  private final ExtendedRemoteLocationConfigurationMappingsRepository extendedMappingsRepository;

  public RemoteLocationConfigurationMapping postRemoteLocationConfigurationMapping(RemoteLocationConfigurationMapping mapping) {
    return mappingsMapper
      .mapEntityToDto(mappingsRepository.save(mappingsMapper.mapDtoToEntity(mapping)));
  }

  public RemoteLocationConfigurationMapping getRemoteLocationConfigurationMapping(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    return mappingsRepository.findById(id)
      .map(mappingsMapper::mapEntityToDto)
      .orElse(null);
  }

  public RemoteLocationConfigurationMappings getRemoteLocationConfigurationMappings(Integer offset, Integer limit) {
    var mappings = mappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return mappingsMapper.mapEntitiesToRemoteLocationConfigurationMappingCollection(mappings);
  }

  public void deleteMappingById(String folioLocationId) {
    var id = UUID.fromString(folioLocationId);
    mappingsRepository.deleteById(id);
  }

  public ExtendedRemoteLocationConfigurationMapping postExtendedRemoteLocationConfigurationMapping(ExtendedRemoteLocationConfigurationMapping mapping) {
    var entity = extendedMappingsRepository.findById(UUID.fromString(mapping.getFinalLocationId()))
      .map(m -> {
        var locations = m.getOriginalLocations();
        var originalLocation = new OriginalLocation();
        originalLocation.setFinalLocationId(UUID.fromString(mapping.getFinalLocationId()));
        originalLocation.setOriginalLocationId(UUID.fromString(mapping.getOriginalLocationId()));
        locations.add(originalLocation);
        m.setOriginalLocations(locations);
        return extendedMappingsRepository.save(m);
      })
      .orElseGet(() -> extendedMappingsRepository.save(ExtendedRemoteLocationConfigurationMappingsMapper.mapDtoToEntity(mapping)));
    return new ExtendedRemoteLocationConfigurationMapping()
      .finalLocationId(entity.getFinalLocationId().toString())
      .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
      .originalLocationId(mapping.getOriginalLocationId());
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(Integer offset, Integer limit) {
    var mappings = extendedMappingsRepository.findAll(new OffsetRequest(offset, limit, Sort.unsorted()));
    return ExtendedRemoteLocationConfigurationMappingsMapper.mapEntitiesToDtoCollection(mappings);
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(String finalLocationId) {
    return extendedMappingsRepository.findById(UUID.fromString(finalLocationId))
      .map(ExtendedRemoteLocationConfigurationMappingsMapper::mapEntityToDtoCollection)
      .orElse(null);
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappingsLocations() {
    var mappings = locationClient.getLocations().getResult().stream()
      .map(folioLocation -> {
        var mapping = originalLocationsRepository
          .findByOriginalLocationId(UUID.fromString(folioLocation.getId()));
        var locationMapping = new ExtendedRemoteLocationConfigurationMapping();
        locationMapping.setOriginalLocationId(folioLocation.getId());
        mapping.ifPresent(m -> locationMapping.setFinalLocationId(m.getFinalLocationId().toString()));
        return locationMapping;
      }).collect(Collectors.toList());
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }
}
