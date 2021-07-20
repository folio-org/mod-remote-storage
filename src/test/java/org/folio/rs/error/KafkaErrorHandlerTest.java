package org.folio.rs.error;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.folio.rs.domain.dto.DomainEvent;
import org.folio.rs.integration.KafkaErrorHandler;
import org.hibernate.exception.SQLGrammarException;
import org.junit.jupiter.api.Test;
import org.springframework.kafka.listener.ListenerExecutionFailedException;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.GenericMessage;


public class KafkaErrorHandlerTest {
  private final KafkaErrorHandler errorHandler = new KafkaErrorHandler();

  @Test
  void shouldThrowTenantInitializationException() {
    var message = buildDomainEvents();
    var exception = buildException(new SQLGrammarException("grammar", new SQLException()));

    assertThatThrownBy(() -> errorHandler.handleError(message, exception))
      .isInstanceOf(TenantNotInitializedException.class)
      .hasMessage("Following tenants might not be initialized yet: [one, two]")
      .hasCauseExactlyInstanceOf(SQLGrammarException.class);
  }

  @Test
  void shouldPropagateOriginalExceptionWhenUnsupportedMessage() {
    var message = buildMessage(Map.of());
    var exception = buildException(new SQLGrammarException("grammar", new SQLException()));

    assertThatThrownBy(() -> errorHandler.handleError(message, exception))
      .isInstanceOf(ListenerExecutionFailedException.class)
      .hasCauseExactlyInstanceOf(SQLGrammarException.class);
  }

  @Test
  void shouldThrowOriginalCauseIfNotTenantInitException() {
    var message = buildDomainEvents();
    var exception = buildException(new IllegalStateException("illegal state exception"));

    assertThatThrownBy(() -> errorHandler.handleError(message, exception))
      .isInstanceOf(ListenerExecutionFailedException.class)
      .hasCauseExactlyInstanceOf(IllegalStateException.class);
  }

  private Message<List<ConsumerRecord<String, DomainEvent>>> buildDomainEvents() {
    var firstEvent = new DomainEvent();
    firstEvent.setTenant("one");
    var secondEvent = new DomainEvent();
    secondEvent.setTenant("two");
    var payloads = List.of(firstEvent,
      secondEvent);

    var consumerRecords = payloads.stream()
      .map(payload -> new ConsumerRecord<>("topic", 1, 0, "key", payload))
      .collect(Collectors.toList());

    return buildMessage(consumerRecords);
  }

  private <T> Message<T> buildMessage(T payload) {
    return new GenericMessage<>(payload);
  }

  private ListenerExecutionFailedException buildException(Throwable cause) {
    return new ListenerExecutionFailedException("exception", cause);
  }
}
