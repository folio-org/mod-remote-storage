package org.folio.rs.client;

import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.okJson;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static org.folio.rs.support.wiremock.WiremockContainerExtension.getWireMockAdminClient;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.extern.log4j.Log4j2;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.rs.domain.dto.Item;
import org.folio.rs.error.ItemReturnException;
import org.folio.rs.support.wiremock.WiremockContainerExtension;
import org.folio.rs.support.wiremock.EnableWiremock;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.config.properties.FolioEnvironment;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.testing.extension.EnablePostgres;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@Log4j2
@EnablePostgres
@ActiveProfiles("test")
@EnableWiremock
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class InventoryClientTest {

  @Autowired private InventoryClient inventoryClient;
  @Autowired private FolioEnvironment folioEnvironment;
  @Autowired private FolioModuleMetadata folioModuleMetadata;
  private String okapiUrl;

  @BeforeEach
  void beforeEach() {
    okapiUrl = WiremockContainerExtension.getWireMockUrl();
    folioEnvironment.setOkapiUrl(okapiUrl);
  }

  @Test
  void getItemByBarcode_found() {
    getWireMockAdminClient().addStubMapping(
      get(urlPathEqualTo("/inventory/items"))
        .withId(UUID.randomUUID())
        .withQueryParam("query", equalTo("barcode==\"foo\\\"bar\""))
        .willReturn(okJson("""
          { "items": [{"id": "nice"}] }
          """))
        .build());
    try (var context = getFolioExecutionContextSetter()) {
      Item item = inventoryClient.getItemByBarcode("foo\"bar");
      assertThat(item.getId(), is("nice"));
    }
  }

  @Test
  void getItemByBarcode_notFound_exception() {
    getWireMockAdminClient().addStubMapping(
      get(urlPathEqualTo("/inventory/items"))
        .withId(UUID.randomUUID())
        .withQueryParam("query", equalTo("barcode==\"other\""))
        .willReturn(okJson("""
          { "items": [], "totalRecords": 0 }
          """))
        .build());
    try (var ignored = getFolioExecutionContextSetter()) {
      assertThrows(ItemReturnException.class, () -> inventoryClient.getItemByBarcode("other"));
    }
  }

  @Test
  void getItemByBarcode_notFound_null() {
    getWireMockAdminClient().addStubMapping(
      get("/inventory/items?query=barcode%3D%3D%22other%22").willReturn(okJson("""
          { "items": [] }
          """))
        .build());
    try (var ignored = getFolioExecutionContextSetter()) {
      assertThat(inventoryClient.getItemByBarcodeOrNull("other"), is(nullValue()));
    }
  }

  public FolioExecutionContextSetter getFolioExecutionContextSetter() {
    var headers = Map.<String, Collection<String>>of(
      XOkapiHeaders.TENANT.toLowerCase(), List.of("rainbow"),
      XOkapiHeaders.URL.toLowerCase(), List.of(okapiUrl));
    var context = new DefaultFolioExecutionContext(folioModuleMetadata, headers);
    return new FolioExecutionContextSetter(context);
  }
}
