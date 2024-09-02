package org.folio.rs;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication
@EnableCaching
@EnableFeignClients
@EnableAutoConfiguration
public class ModRemoteStorageApplication {
  public static void main(String[] args) {
    SpringApplication.run(ModRemoteStorageApplication.class, args);
  }
}
