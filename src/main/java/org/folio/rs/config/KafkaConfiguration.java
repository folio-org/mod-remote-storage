package org.folio.rs.config;

import static org.apache.kafka.clients.consumer.ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG;

import java.util.HashMap;

import org.apache.kafka.common.serialization.StringDeserializer;
import org.folio.rs.config.properties.FolioKafkaProperties;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.domain.dto.PubSubEvent;
import org.springframework.boot.kafka.autoconfigure.KafkaProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.listener.DefaultErrorHandler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.support.serializer.JacksonJsonDeserializer;

@Slf4j
@Configuration
@RequiredArgsConstructor
@EnableConfigurationProperties({ FolioKafkaProperties.class })
public class KafkaConfiguration {

  private final KafkaProperties kafkaProperties;

  @Bean
  public ConcurrentKafkaListenerContainerFactory<String, DomainEvent> kafkaListenerContainerFactory() {
    var factory = new ConcurrentKafkaListenerContainerFactory<String, DomainEvent>();
    factory.setBatchListener(true);
    factory.setConsumerFactory(jsonNodeConsumerFactory());
    factory.setCommonErrorHandler(new DefaultErrorHandler((consumerRecord, exception) -> log.error(
      "Error processing Kafka record [topic: {}, partition: {}, offset: {}]",
      consumerRecord.topic(), consumerRecord.partition(), consumerRecord.offset(), exception)));
    return factory;
  }

  private ConsumerFactory<String, DomainEvent> jsonNodeConsumerFactory() {
    var deserializer = new JacksonJsonDeserializer<>(DomainEvent.class);
    var config = new HashMap<>(kafkaProperties.buildConsumerProperties());
    config.put(KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
    config.put(VALUE_DESERIALIZER_CLASS_CONFIG, deserializer);
    return new DefaultKafkaConsumerFactory<>(config, new StringDeserializer(), deserializer);
  }

  @Bean
  public ConcurrentKafkaListenerContainerFactory<String, PubSubEvent> kafkaPubSubListenerContainerFactory() {
    var factory = new ConcurrentKafkaListenerContainerFactory<String, PubSubEvent>();
    factory.setBatchListener(true);
    factory.setConsumerFactory(pubSubEventConsumerFactory());
    factory.setCommonErrorHandler(new DefaultErrorHandler((consumerRecord, exception) -> log.error(
      "Error processing LOG_RECORD Kafka record [topic: {}, partition: {}, offset: {}]",
      consumerRecord.topic(), consumerRecord.partition(), consumerRecord.offset(), exception)));
    return factory;
  }

  private ConsumerFactory<String, PubSubEvent> pubSubEventConsumerFactory() {
    var deserializer = new JacksonJsonDeserializer<>(PubSubEvent.class);
    var config = new HashMap<>(kafkaProperties.buildConsumerProperties());
    config.put(KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
    config.put(VALUE_DESERIALIZER_CLASS_CONFIG, deserializer);
    return new DefaultKafkaConsumerFactory<>(config, new StringDeserializer(), deserializer);
  }

}
