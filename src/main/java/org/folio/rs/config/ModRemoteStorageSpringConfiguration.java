package org.folio.rs.config;

import feign.Logger;
import org.folio.rs.client.EnrichHeadersClient;
import org.folio.rs.client.logger.SensitiveDataProtectionLogger;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import feign.Client;

@Configuration
public class ModRemoteStorageSpringConfiguration {

  @Bean
  public Client enrichHeadersClient() {
    return new EnrichHeadersClient();
  }

  @Bean
  public Logger feignLogger() {
    return new SensitiveDataProtectionLogger();
  }
}
