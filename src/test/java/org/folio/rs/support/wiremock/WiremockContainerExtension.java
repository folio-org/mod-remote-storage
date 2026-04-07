package org.folio.rs.support.wiremock;

import com.github.tomakehurst.wiremock.client.HttpAdminClient;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.core.Admin;
import lombok.NonNull;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.utility.MountableFile;

@Slf4j
public class WiremockContainerExtension implements BeforeAllCallback, AfterAllCallback {

  public static final int WM_DOCKER_PORT = 8080;
  public static final String FOLIO_OKAPI_URL_PROPERTY = "folio.okapi-url";

  private static final DockerImageName WM_IMAGE = DockerImageName.parse("wiremock/wiremock:3.13.2");

  private static Admin adminClient;
  private static WireMock client;

  @SuppressWarnings("resource")
  private static final GenericContainer<?> WM_CONTAINER = new GenericContainer<>(WM_IMAGE)
    .withExposedPorts(WM_DOCKER_PORT)
    .withAccessToHost(true)
    .withCommand("--local-response-templating", "--disable-banner")
    .withCopyFileToContainer(MountableFile.forClasspathResource("mappings"), "/home/wiremock/mappings")
    .withLogConsumer(new Slf4jLogConsumer(log).withSeparateOutputStreams());

  public static Admin getWireMockAdminClient() {
    if (adminClient == null) {
      throw new IllegalStateException("WireMock admin client isn't initialized");
    }
    return adminClient;
  }

  public static WireMock getWireMockClient() {
    if (client == null) {
      throw new IllegalStateException("WireMock client isn't initialized");
    }
    return client;
  }

  public static String getWireMockUrl() {
    return String.format("http://%s:%s",
      WM_CONTAINER.getHost(),
      WM_CONTAINER.getMappedPort(WM_DOCKER_PORT));
  }

  @Override
  public void beforeAll(@NonNull ExtensionContext context) {
    runContainer();
    System.setProperty(FOLIO_OKAPI_URL_PROPERTY, getWireMockUrl());
    getWireMockAdminClient().resetAll();
  }

  @SneakyThrows
  private static void runContainer() {
    if (WM_CONTAINER.isRunning()) {
      return;
    }

    WM_CONTAINER.start();
    log.info("WireMock container started [url: {}]", getWireMockUrl());

    var host = WM_CONTAINER.getHost();
    var port = WM_CONTAINER.getMappedPort(WM_DOCKER_PORT);
    adminClient = new HttpAdminClient(host, port);
    client = new WireMock(adminClient);
    WireMock.configureFor(host, port);
  }

  @Override
  public void afterAll(@NonNull ExtensionContext context) {
    getWireMockAdminClient().resetAll();
  }
}
