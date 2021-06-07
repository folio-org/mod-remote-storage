package org.folio.rs.service;

import static java.util.Objects.isNull;
import static java.util.Optional.ofNullable;
import static org.folio.rs.util.MapperUtils.stringToUUIDSafe;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.rs.client.LocationClient;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.ExtendedRemoteLocationConfigurationMappings;
import org.folio.rs.domain.dto.LocationMappingFilterData;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMapping;
import org.folio.rs.domain.dto.RemoteLocationConfigurationMappings;
import org.folio.rs.domain.entity.ExtendedRemoteLocationConfigurationMappingEntity;
import org.folio.rs.domain.entity.ExtendedRemoteLocationConfigurationMappingEntity_;
import org.folio.rs.domain.entity.OriginalLocation;
import org.folio.rs.domain.entity.OriginalLocation_;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity;
import org.folio.rs.domain.entity.RemoteLocationConfigurationMappingEntity_;
import org.folio.rs.mapper.RemoteLocationConfigurationMappingsMapper;
import org.folio.rs.mapper.ExtendedRemoteLocationConfigurationMappingsMapper;
import org.folio.rs.repository.ExtendedRemoteLocationConfigurationMappingsRepository;
import org.folio.rs.repository.OriginalLocationsRepository;
import org.folio.rs.repository.RemoteLocationConfigurationMappingsRepository;
import org.folio.spring.data.OffsetRequest;
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
  private final RemoteLocationConfigurationMappingsRepository mappingsRepository;
  private final RemoteLocationConfigurationMappingsMapper mappingsMapper;
  private final LocationClient locationClient;
  private final OriginalLocationsRepository originalLocationsRepository;
  private final ExtendedRemoteLocationConfigurationMappingsRepository extendedMappingsRepository;

  public RemoteLocationConfigurationMapping postRemoteLocationConfigurationMapping(RemoteLocationConfigurationMapping mapping) {
    return mappingsMapper
      .mapEntityToDto(mappingsRepository.save(mappingsMapper.mapDtoToEntity(mapping)));
  }

  public RemoteLocationConfigurationMappings getRemoteLocationConfigurationMapping(LocationMappingFilterData filterData) {
    var entities = mappingsRepository.findAll(getMappingSpecification(filterData),
      new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return mappingsMapper.mapEntitiesToRemoteLocationConfigurationMappingCollection(entities);
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
        m.setRemoteConfigurationId(UUID.fromString(mapping.getRemoteConfigurationId()));
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

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(LocationMappingFilterData filterData) {
    var mappings = extendedMappingsRepository.findAll(getExtendedMappingSpecification(filterData),
      new OffsetRequest(filterData.getOffset(), filterData.getLimit(), Sort.unsorted()));
    return ExtendedRemoteLocationConfigurationMappingsMapper.mapEntitiesToDtoCollection(mappings, filterData.getOriginalLocationId());
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappings(String finalLocationId) {
    return extendedMappingsRepository.findById(UUID.fromString(finalLocationId))
      .map(ExtendedRemoteLocationConfigurationMappingsMapper::mapEntityToDtoCollection)
      .orElse(null);
  }

  public ExtendedRemoteLocationConfigurationMappings getExtendedRemoteLocationConfigurationMappingsLocations(LocationMappingFilterData filterData) {
    var mappings = locationClient.getLocations().getResult().stream()
      .filter(folioLocation -> isNull(filterData.getOriginalLocationId()) || filterData.getOriginalLocationId().equals(folioLocation.getId()))
      .map(folioLocation -> {
        var locationMappings = getExtendedRemoteLocationConfigurationMappings(LocationMappingFilterData
          .builder()
          .originalLocationId(folioLocation.getId())
          .limit(Integer.MAX_VALUE)
          .build());
        return locationMappings.getMappings().isEmpty() ?
          new ExtendedRemoteLocationConfigurationMapping().originalLocationId(folioLocation.getId()) :
          locationMappings.getMappings().get(0);
      })
      .filter(lm -> isNull(filterData.getRemoteStorageId()) || filterData.getRemoteStorageId().equals(lm.getRemoteConfigurationId()))
      .filter(lm -> isNull(filterData.getFinalLocationId()) || filterData.getFinalLocationId().equals(lm.getFinalLocationId()))
      .collect(Collectors.toList());
    return new ExtendedRemoteLocationConfigurationMappings()
      .mappings(mappings)
      .totalRecords(mappings.size());
  }

  private Specification<RemoteLocationConfigurationMappingEntity> getMappingSpecification(LocationMappingFilterData filterData) {
    return (rec, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      ofNullable(filterData.getFinalLocationId())
        .ifPresent(id -> predicates.add(builder.equal(rec.get(RemoteLocationConfigurationMappingEntity_.finalLocationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getRemoteStorageId())
        .ifPresent(id -> predicates.add(builder.equal(rec.get(RemoteLocationConfigurationMappingEntity_.remoteConfigurationId), stringToUUIDSafe(id))));
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }

  private Specification<ExtendedRemoteLocationConfigurationMappingEntity> getExtendedMappingSpecification(LocationMappingFilterData filterData) {
    return (root, criteriaQuery, builder) -> {
      final Collection<Predicate> predicates = new ArrayList<>();
      ofNullable(filterData.getFinalLocationId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(ExtendedRemoteLocationConfigurationMappingEntity_.finalLocationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getRemoteStorageId())
        .ifPresent(id -> predicates.add(builder.equal(root.get(ExtendedRemoteLocationConfigurationMappingEntity_.remoteConfigurationId), stringToUUIDSafe(id))));
      ofNullable(filterData.getOriginalLocationId())
        .ifPresent(id -> {
          var join = root.join(ExtendedRemoteLocationConfigurationMappingEntity_.originalLocations);
          var predicate = builder.equal(join.get(OriginalLocation_.originalLocationId), stringToUUIDSafe(id));
          criteriaQuery.distinct(true);
          predicates.add(predicate);
        });
      return builder.and(predicates.toArray(new Predicate[0]));
    };
  }
}
