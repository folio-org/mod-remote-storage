package org.folio.rs.service;


import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;
import lombok.extern.log4j.Log4j2;
import org.folio.rs.domain.dto.Provider;
import org.folio.rs.domain.entity.ProviderRecord;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class ProvidersService {

  private final List<Provider> providers;

  public ProvidersService() {
    this.providers = new ArrayList<>();
    prepareProviderData();
  }

  public List<Provider> getProviders() {
    return providers;
  }

  private void prepareProviderData() {
    asList(ProviderRecord.values()).forEach(provider -> {
      Provider providerData = new Provider();
      providerData.setName(provider.getName());
      providerData.setId(provider.getId());
      providers.add(providerData);
    });
  }
}

