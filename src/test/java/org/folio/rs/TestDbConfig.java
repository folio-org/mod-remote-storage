package org.folio.rs;

import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;

@Configuration
public class TestDbConfig {

  @Bean
  DataSource testDataSource() {
    return DataSourceBuilder.create().build();
  }
}
