package org.folio.rs.config.properties;

import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties("application.kafka")
public class FolioKafkaProperties {

  private Map<String, KafkaListenerProperties> listener;

  @Data
  public static class KafkaListenerProperties {
    private String topics;
    private String concurrency;
    private String groupId;
  }
}
