package org.folio.rs;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
@EnableCaching
public class ModRemoteStorageApplication {
  public static void main(String[] args) {
    SpringApplication.run(ModRemoteStorageApplication.class, args);
  }
}
