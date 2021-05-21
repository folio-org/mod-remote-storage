package org.folio.rs;

import javax.sql.DataSource;

import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestDbConfig {

  @Bean
  DataSource testDataSource() {
    return DataSourceBuilder.create().build();
  }
}
