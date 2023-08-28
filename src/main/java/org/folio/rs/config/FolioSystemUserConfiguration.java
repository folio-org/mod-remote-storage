package org.folio.rs.config;

import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"org.folio.spring.client", "org.folio.spring.context",
  "org.folio.spring.service", "org.folio.spring.config.properties"})
@EnableFeignClients(basePackages = "org.folio.spring.client")
public class FolioSystemUserConfiguration {
}
