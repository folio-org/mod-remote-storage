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
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.domain.entity.OriginalLocation_;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity_;
import org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.folio.rs.repository.OriginalLocationsRepository;
import org.folio.spring.data.OffsetRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import javax.persistence.criteria.Predicate;
import javax.transaction.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class LocationMappingsService {
  public static final String MAPPINGS = "mappings";
  private final LocationClient locationClient;
  private final OriginalLocationsRepository originalLocationsRepository;
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

  @Transactional(Transactional.TxType.REQUIRES_NEW)
  public void deleteOriginalLocationByIdAndFinalLocationId(String finalLocationId, String originalLocationId) {
    originalLocationsRepository.deleteByFinalLocationIdAndAndOriginalLocationId(UUID.fromString(finalLocationId), UUID.fromString(originalLocationId));
  }

  public ExtendedRemoteLocationConfigurationMapping postExtendedRemoteLocationConfigurationMapping(ExtendedRemoteLocationConfigurationMapping mapping) {
    var existingEntity = extendedMappingsRepository.findById((UUID.fromString(mapping.getFinalLocationId())));
    RemoteLocationConfigurationMappingEntity entity;
    if (existingEntity.isPresent()) {
      entity = extendedMappingsRepository.save(buildNewMappingEntity(existingEntity.get(), mapping));
    } else {
      var existingEntities = getExtendedRemoteLocationConfigurationMappingEntities(LocationMappingFilterData
        .builder()
        .originalLocationId(mapping.getOriginalLocationId())
        .remoteStorageId(mapping.getRemoteConfigurationId())
        .build())
        .getContent();
      if (existingEntities.isEmpty()) {
        entity = extendedMappingsRepository.save(RemoteLocationConfigurationMappingsMapper.mapExtendedMappingDtoToEntity(mapping));
      } else {
        entity = extendedMappingsRepository.save(buildNewMappingEntity(existingEntities.get(0), mapping));
        extendedMappingsRepository.delete(existingEntities.get(0));
      }
    }
    return new ExtendedRemoteLocationConfigurationMapping()
      .finalLocationId(entity.getFinalLocationId().toString())
      .remoteConfigurationId(entity.getRemoteConfigurationId().toString())
      .originalLocationId(mapping.getOriginalLocationId());
  }

  private RemoteLocationConfigurationMappingEntity buildNewMappingEntity(RemoteLocationConfigurationMappingEntity entity,
    ExtendedRemoteLocationConfigurationMapping dto) {

    var newEntity = new RemoteLocationConfigurationMappingEntity();

    newEntity.setFinalLocationId(UUID.fromString(dto.getFinalLocationId()));
    newEntity.setRemoteConfigurationId(UUID.fromString(dto.getRemoteConfigurationId()));

    var locations = entity.getOriginalLocations();
    var existingOriginalLocationIds = locations.stream()
      .map(OriginalLocation::getOriginalLocationId)
      .map(UUID::toString)
      .collect(Collectors.toSet());
    if (!existingOriginalLocationIds.contains(dto.getOriginalLocationId())) {
      var originalLocation = new OriginalLocation();
      originalLocation.setFinalLocationId(UUID.fromString(dto.getFinalLocationId()));
      originalLocation.setOriginalLocationId(UUID.fromString(dto.getOriginalLocationId()));
      locations.add(originalLocation);
      newEntity.setOriginalLocations(locations);
    } else {
      newEntity.setOriginalLocations(entity.getOriginalLocations());
    }

    return newEntity;
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
      .filter(lm -> isNull(filterData.getRemoteStorageId()) || filterData.getRemoteStorageId().equals(lm.getRemoteConfigurationId()))
      .filter(lm -> isNull(filterData.getFinalLocationId()) || filterData.getFinalLocationId().equals(lm.getFinalLocationId()))
      .collect(Collectors.toList());

    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(paginate(mappings, filterData.getOffset(), filterData.getLimit()))
      .totalRecords(mappings.size());
  }

  private List<ExtendedRemoteLocationConfigurationMapping> paginate(List<ExtendedRemoteLocationConfigurationMapping> mappings, int offset, int limit) {
    var _offset = Math.min(offset, mappings.size());
    var _limit = Math.min(_offset + limit, mappings.size());
    return mappings.subList(_offset, _limit);
  }

  private Specification<RemoteLocationConfigurationMappingEntity> getExtendedMappingSpecification(LocationMappingFilterData filterData) {
    return (root, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      ofNullable(filterData.getFinalLocationId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(RemoteLocationConfigurationMappingEntity_.finalLocationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getRemoteStorageId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(RemoteLocationConfigurationMappingEntity_.remoteConfigurationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getOriginalLocationId())
        .ifPresent(id -> {
          var join = root.join(RemoteLocationConfigurationMappingEntity_.originalLocations);
          var predicate = builder.equal(join.get(OriginalLocation_.originalLocationId), stringToUUIDSafe(id));
          criteriaQuery.distinct(true);
          predicates.add(predicate);
        });
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }
}
