package org.folio.rs.service;

import static java.lang.String.format;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.rest.jaxrs.model.MessagingModule.ModuleRole.PUBLISHER;
import static org.folio.rest.jaxrs.model.MessagingModule.ModuleRole.SUBSCRIBER;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.folio.HttpStatus;
import org.folio.rest.client.PubsubClient;
import org.folio.rest.jaxrs.model.EventDescriptor;
import org.folio.rest.jaxrs.model.MessagingDescriptor;
import org.folio.rest.jaxrs.model.MessagingModule;
import org.folio.rest.jaxrs.model.PublisherDescriptor;
import org.folio.rest.jaxrs.model.SubscriberDescriptor;
import org.folio.rs.error.PubSubException;
import org.folio.util.pubsub.support.DescriptorHolder;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.extern.log4j.Log4j2;

/**
  Temporary solution for fixing issue with PomReader from RMB
 */
@Component
@Log4j2
public class PubSubService {

  private static final String MESSAGING_CONFIG_FILE_NAME = "MessagingDescriptor.json";
  public static final String POM_XML = "pom.xml";

  public boolean registerPubSubModule(String okapiUrl, String tenantId, String token) {
    boolean isCompleted = false;
    PubsubClient client = new PubsubClient(okapiUrl, tenantId, token);
    try {
      log.info("Reading MessagingDescriptor.json");
      DescriptorHolder descriptorHolder = readMessagingDescriptor();
      if (descriptorHolder.getPublisherDescriptor() != null && isNotEmpty(descriptorHolder.getPublisherDescriptor().getEventDescriptors())) {
        log.info("Registering events for publishers");
        List<EventDescriptor> eventDescriptors = descriptorHolder.getPublisherDescriptor().getEventDescriptors();
        registerEventTypes(client, eventDescriptors);
        registerPubSubPublishers(client, descriptorHolder.getPublisherDescriptor());
        isCompleted = true;
      }
      if (descriptorHolder.getSubscriberDescriptor() != null && isNotEmpty(descriptorHolder.getSubscriberDescriptor().getSubscriptionDefinitions())) {
        registerPubSubSubscribers(client, descriptorHolder.getSubscriberDescriptor());
        isCompleted = true;
      }
    } catch (Exception e) {
      throw new PubSubException("Error during registration module in PubSub", e);
    }
    return isCompleted;
  }

  private void registerEventTypes(PubsubClient client, List<EventDescriptor> events) {
    try {
      for (EventDescriptor eventDescriptor : events) {
        client.postPubsubEventTypes(null, eventDescriptor, result -> {
          if (result.result().statusCode() != HttpStatus.HTTP_CREATED.toInt()) {
            throw new PubSubException(format("EventDescriptor was not registered for eventType: %s . Status code: %s", eventDescriptor.getEventType(), result.result().statusCode()));
          }
        });
      }
    } catch (Exception e) {
      throw new PubSubException("Module's events were not registered in PubSub.", e);
    }
  }

  private boolean registerPubSubSubscribers(PubsubClient client, SubscriberDescriptor descriptor) {
    log.info("Registering module's subscribers");
    var isCompleted = new AtomicBoolean(false);
    try {
      client.postPubsubEventTypesDeclareSubscriber(descriptor, result -> {
        if (result.result().statusCode() == HttpStatus.HTTP_CREATED.toInt()) {
          log.info("Module's subscribers were successfully registered");
          isCompleted.set(true);
        } else {
          throw new PubSubException("Module's subscribers were not registered in PubSub. HTTP status: " + result.result().statusCode());
        }
      });
    } catch (Exception e) {
      throw new PubSubException("Module's subscribers were not registered in PubSub.", e);
    }
    return isCompleted.get();
  }

  private boolean registerPubSubPublishers(PubsubClient client, PublisherDescriptor descriptor) {
    log.info("Registering module's publishers");
    var isCompleted = new AtomicBoolean(false);
    try {
      client.postPubsubEventTypesDeclarePublisher(descriptor, result -> {
        if (result.result().statusCode() == HttpStatus.HTTP_CREATED.toInt()) {
          log.info("Module's publishers were successfully registered");
          isCompleted.set(true);
        } else {
          throw new PubSubException("Module's publishers were not registered in PubSub. HTTP status: " + result.result().statusCode());
        }
      });
    } catch (Exception e) {
      throw new PubSubException("Module's publishers were not registered in PubSub.", e);
    }
    return isCompleted.get();
  }

  public boolean unregisterPubSubModule(String okapiUrl, String tenantId, String token) {
    PubsubClient client = new PubsubClient(okapiUrl, tenantId, token);
    String moduleId = constructModuleName();
    return unregisterModuleByIdAndRole(client, moduleId, PUBLISHER) && unregisterModuleByIdAndRole(client, moduleId, SUBSCRIBER);
  }

  private boolean unregisterModuleByIdAndRole(PubsubClient client, String moduleId, MessagingModule.ModuleRole moduleRole) {
    AtomicBoolean isComplete = new AtomicBoolean(false);
    try {
      log.info("Trying to unregister module with name '{}' as {}", moduleId, moduleRole);
      client.deletePubsubMessagingModules(moduleId, moduleRole.value(), response -> {
        if (response.result().statusCode() == HttpStatus.HTTP_NO_CONTENT.toInt()) {
          log.info("Module {} was successfully unregistered as '{}'", moduleId, moduleRole);
          isComplete.set(true);
        } else {
          throw new PubSubException(format("Module %s was not unregistered as '%s' in PubSub. HTTP status: %s", moduleId, moduleRole, response.result().statusCode()));
        }
      });
    } catch (Exception e) {
      throw new PubSubException(String.format("Module was not unregistered as %s in PubSub.", moduleRole), e);
    }
    return isComplete.get();
  }

  private DescriptorHolder readMessagingDescriptor() throws IOException {
    ObjectMapper objectMapper = new ObjectMapper();
    try {
      MessagingDescriptor messagingDescriptor = objectMapper.readValue(getMessagingDescriptorInputStream(), MessagingDescriptor.class);
      return new DescriptorHolder().withPublisherDescriptor(new PublisherDescriptor().withModuleId(constructModuleName())
        .withEventDescriptors(messagingDescriptor.getPublications()))
        .withSubscriberDescriptor(new SubscriberDescriptor().withModuleId(constructModuleName())
          .withSubscriptionDefinitions(messagingDescriptor.getSubscriptions()));
    } catch (JsonParseException | JsonMappingException e) {
      throw new PubSubException("Can not read messaging descriptor, cause: " + e.getMessage());
    }
  }

  private InputStream getMessagingDescriptorInputStream() {
    return getFileInputStreamFromClassPath(MESSAGING_CONFIG_FILE_NAME)
      .orElseThrow(() -> new PubSubException("Messaging descriptor file 'MessagingDescriptor.json' not found"));
  }

  private Optional<InputStream> getFileInputStreamFromClassPath(String path) {
    String preparedPath = path.replace('\\', '/');
    InputStream fis = PubSubService.class.getClassLoader().getResourceAsStream(preparedPath);
    if (fis == null) {
      return Optional.empty();
    }
    return Optional.of(fis);
  }

  public String constructModuleName() {
    var xmlMapper = new XmlMapper();
    try {
      var json = xmlMapper.readTree(new File(POM_XML));
      return String.format("%s-%s", json.get("artifactId").asText(), json.get("version").asText());
    } catch (Exception e) {
      throw new PubSubException("Error in constructing module name", e);
    }
  }
}
