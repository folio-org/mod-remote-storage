package org.folio.rs.service;

import static java.util.Objects.isNull;
import static java.util.Optional.ofNullable;
import static org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper.mapEntityToMappingDto;
import static org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper.mapMappingDtoToEntity;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity_;
import org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import javax.persistence.criteria.Predicate;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  public static final String MAPPINGS = "mappings";
  private final LocationClient locationClient;
  private final ExtendedRemoteLocationConfigurationMappingsRepository extendedMappingsRepository;

  public RemoteLocationConfigurationMapping postRemoteLocationConfigurationMapping(RemoteLocationConfigurationMapping mapping) {
    return mapEntityToMappingDto(extendedMappingsRepository.save(mapMappingDtoToEntity(mapping)));
  }

  public RemoteLocationConfigurationMappings getRemoteLocationConfigurationMappings(LocationMappingFilterData filterData) {
    var mappings = extendedMappingsRepository.findAll(getExtendedMappingSpecification(filterData),
      new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return RemoteLocationConfigurationMappingsMapper.mapEntitiesToMappingsDtoCollection(mappings);
  }

  public RemoteLocationConfigurationMapping getRemoteLocationConfigurationMapping(String folioLocationId) {
    return extendedMappingsRepository.findById(UUID.fromString(folioLocationId))
      .map(RemoteLocationConfigurationMappingsMapper::mapEntityToMappingDto)
      .orElse(null);
  }

  public void deleteMappingById(String finalLocationId) {
    extendedMappingsRepository.deleteById(UUID.fromString(finalLocationId));
  }

  public void deleteOriginalLocationByIdAndFinalLocationId(String finalLocationId, String originalLocationId) {
    extendedMappingsRepository.findById(UUID.fromString(finalLocationId))
      .ifPresent(entity -> {
        var originalLocationIds = entity.getOriginalLocationIds();
        originalLocationIds.remove(UUID.fromString(originalLocationId));
        entity.setOriginalLocationIds(originalLocationIds);
        extendedMappingsRepository.save(entity);
      });
  }

  public ExtendedRemoteLocationConfigurationMapping postExtendedRemoteLocationConfigurationMapping(ExtendedRemoteLocationConfigurationMapping mapping) {
    removeOriginalLocationIdFromExistingEntities(mapping);
    var entity = extendedMappingsRepository.findById(UUID.fromString(mapping.getFinalLocationId()))
      .map(e -> extendedMappingsRepository.save(updateEntityFromDto(e, mapping)))
      .orElseGet(() -> extendedMappingsRepository.save(RemoteLocationConfigurationMappingsMapper.mapExtendedMappingDtoToEntity(mapping)));

    return new ExtendedRemoteLocationConfigurationMapping()
      .finalLocationId(entity.getFinalLocationId().toString())
      .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
      .originalLocationId(mapping.getOriginalLocationId());
  }

  private void removeOriginalLocationIdFromExistingEntities(ExtendedRemoteLocationConfigurationMapping mapping) {
    getExtendedRemoteLocationConfigurationMappingEntities(LocationMappingFilterData
      .builder()
      .originalLocationId(mapping.getOriginalLocationId())
      .remoteConfigurationId(mapping.getRemoteConfigurationId())
      .build())
      .getContent()
      .forEach(e -> {
        var originalLocationIds = e.getOriginalLocationIds();
        originalLocationIds.remove(UUID.fromString(mapping.getOriginalLocationId()));
        e.setOriginalLocationIds(originalLocationIds);
        extendedMappingsRepository.save(e);
      });
  }

  private RemoteLocationConfigurationMappingEntity updateEntityFromDto(RemoteLocationConfigurationMappingEntity entity,
    ExtendedRemoteLocationConfigurationMapping dto) {
    entity.setRemoteConfigurationId(UUID.fromString(dto.getRemoteConfigurationId()));
    var originalLocations = entity.getOriginalLocationIds();
    originalLocations.add(UUID.fromString(dto.getOriginalLocationId()));
    entity.setOriginalLocationIds(originalLocations);
    return entity;
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(LocationMappingFilterData filterData) {
    var mappings = getExtendedRemoteLocationConfigurationMappingEntities(filterData);
    return RemoteLocationConfigurationMappingsMapper.mapEntitiesToExtendedMappingsDtoCollection(mappings, filterData.getOriginalLocationId());
  }

  public Page<RemoteLocationConfigurationMappingEntity> getExtendedRemoteLocationConfigurationMappingEntities(LocationMappingFilterData filterData) {
    return extendedMappingsRepository.findAll(getExtendedMappingSpecification(filterData),
      new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(String finalLocationId) {
    return extendedMappingsRepository.findById(UUID.fromString(finalLocationId))
      .map(RemoteLocationConfigurationMappingsMapper::mapEntityToExtendedMappingDtoCollection)
      .orElse(null);
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappingsLocations(LocationMappingFilterData filterData) {
    var remoteIds = getRemoteLocationConfigurationMappings(LocationMappingFilterData.builder().build())
      .getMappings().stream()
      .map(RemoteLocationConfigurationMapping::getFolioLocationId)
      .collect(Collectors.toSet());
    var mappings = locationClient.getLocations(0, Integer.MAX_VALUE).getResult().stream()
      .filter(folioLocation -> !remoteIds.contains(folioLocation.getId()))
      .filter(folioLocation -> isNull(filterData.getOriginalLocationId()) || filterData.getOriginalLocationId().equals(folioLocation.getId()))
      .map(folioLocation -> {
        var locationMappings = getExtendedRemoteLocationConfigurationMappings(LocationMappingFilterData
          .builder()
          .originalLocationId(folioLocation.getId())
          .build());
        return locationMappings.getMappings().isEmpty() ?
          Collections.singletonList(new ExtendedRemoteLocationConfigurationMapping().originalLocationId(folioLocation.getId())) :
          locationMappings.getMappings();
      })
      .flatMap(List::stream)
      .filter(lm -> isNull(lm.getFinalLocationId()) || isNull(filterData.getRemoteConfigurationId()) || filterData.getRemoteConfigurationId().equals(lm.getRemoteConfigurationId()))
      .filter(lm -> isNull(lm.getFinalLocationId()) || isNull(filterData.getFinalLocationId()) || filterData.getFinalLocationId().equals(lm.getFinalLocationId()))
      .collect(Collectors.toList());

    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(paginate(mappings, filterData.getOffset(), filterData.getLimit()))
      .totalRecords(mappings.size());
  }

  private List<ExtendedRemoteLocationConfigurationMapping> paginate(List<ExtendedRemoteLocationConfigurationMapping> mappings, int offset, int limit) {
    var o = Math.min(offset, mappings.size());
    var l = Math.min(o + limit, mappings.size());
    return mappings.subList(o, l);
  }

  private Specification<RemoteLocationConfigurationMappingEntity> getExtendedMappingSpecification(LocationMappingFilterData filterData) {
    return (root, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      ofNullable(filterData.getFinalLocationId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(RemoteLocationConfigurationMappingEntity_.finalLocationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getRemoteConfigurationId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(RemoteLocationConfigurationMappingEntity_.remoteConfigurationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getOriginalLocationId())
        .ifPresent(id -> predicates.add(builder.isMember(UUID.fromString(id), root.get(RemoteLocationConfigurationMappingEntity_.originalLocationIds))));
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }
}
