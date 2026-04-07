package org.folio.rs.config;

import lombok.RequiredArgsConstructor;
import org.folio.rs.client.CirculationClient;
import org.folio.rs.client.ContributorTypesClient;
import org.folio.rs.client.HoldingsStorageClient;
import org.folio.rs.client.IdentifierTypesClient;
import org.folio.rs.client.InventoryClient;
import org.folio.rs.client.ItemNoteTypesClient;
import org.folio.rs.client.LocationClient;
import org.folio.rs.client.RemoteStorageUsersClient;
import org.folio.rs.client.ServicePointsClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;

@Configuration
@RequiredArgsConstructor
public class HttpExchangeConfiguration {

  private final HttpServiceProxyFactory factory;

  /**
   * Creates a {@link CirculationClient} bean.
   *
   * @return the {@link CirculationClient} instance
   */
  @Bean
  public CirculationClient circulationClient() {
    return factory.createClient(CirculationClient.class);
  }

  /**
   * Creates a {@link ContributorTypesClient} bean.
   *
   * @return the {@link ContributorTypesClient} instance
   */
  @Bean
  public ContributorTypesClient contributorTypesClient() {
    return factory.createClient(ContributorTypesClient.class);
  }

  /**
   * Creates a {@link HoldingsStorageClient} bean.
   *
   * @return the {@link HoldingsStorageClient} instance
   */
  @Bean
  public HoldingsStorageClient holdingsStorageClient() {
    return factory.createClient(HoldingsStorageClient.class);
  }

  /**
   * Creates a {@link IdentifierTypesClient} bean.
   *
   * @return the {@link IdentifierTypesClient} instance
   */
  @Bean
  public IdentifierTypesClient identifierTypesClient() {
    return factory.createClient(IdentifierTypesClient.class);
  }

  /**
   * Creates a {@link InventoryClient} bean.
   *
   * @return the {@link InventoryClient} instance
   */
  @Bean
  public InventoryClient inventoryClient() {
    return factory.createClient(InventoryClient.class);
  }

  /**
   * Creates a {@link ItemNoteTypesClient} bean.
   *
   * @return the {@link ItemNoteTypesClient} instance
   */
  @Bean
  public ItemNoteTypesClient itemNoteTypesClient() {
    return factory.createClient(ItemNoteTypesClient.class);
  }

  /**
   * Creates a {@link LocationClient} bean.
   *
   * @return the {@link LocationClient} instance
   */
  @Bean
  public LocationClient locationClient() {
    return factory.createClient(LocationClient.class);
  }

  /**
   * Creates a {@link RemoteStorageUsersClient} bean.
   *
   * @return the {@link RemoteStorageUsersClient} instance
   */
  @Bean
  public RemoteStorageUsersClient remoteStorageUsersClient() {
    return factory.createClient(RemoteStorageUsersClient.class);
  }

  /**
   * Creates a {@link ServicePointsClient} bean.
   *
   * @return the {@link ServicePointsClient} instance
   */
  @Bean
  public ServicePointsClient servicePointsClient() {
    return factory.createClient(ServicePointsClient.class);
  }
}
