package org.folio.rs;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import io.zonky.test.db.provider.postgres.PostgreSQLContainerCustomizer;

@Configuration
public class EmbeddedPostgresConfiguration {
  private static final String IMAGE_NAME =
      System.getenv().getOrDefault("TESTCONTAINERS_POSTGRES_IMAGE", "postgres:16-alpine");

  @Bean
  public PostgreSQLContainerCustomizer postgresContainerCustomizer() {
    return container -> container.setDockerImageName(IMAGE_NAME);
  }
}
