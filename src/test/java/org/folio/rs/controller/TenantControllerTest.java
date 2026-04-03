package org.folio.rs.controller;

import static org.folio.rs.support.wiremock.WiremockContainerExtension.getWireMockUrl;
import static org.mockito.Mockito.verify;

import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.rs.support.wiremock.EnableWiremock;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.controller.TenantController;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.testing.extension.EnablePostgres;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;

@EnableWiremock
@EnablePostgres
@ActiveProfiles("test")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class TenantControllerTest {

  private final static String DROP_SCHEMA_QUERY = "DROP SCHEMA IF EXISTS %1$s CASCADE; DROP ROLE IF EXISTS %1$s";

  @Autowired
  private TenantController tenantController;

  @Autowired
  private FolioModuleMetadata moduleMetadata;

  @Autowired
  private FolioExecutionContext context;

  @MockitoSpyBean
  private JdbcTemplate jdbcTemplate;

  @Test
  void shouldDropDatabaseSchemaUponTenantDeletion() {
    try (var ignored = new FolioExecutionContextSetter(AsyncFolioExecutionContext.builder()
      .tenantId("test_tenant")
      .moduleMetadata(moduleMetadata)
      .okapiUrl(getWireMockUrl()).build())) {
      tenantController.postTenant(new TenantAttributes().moduleTo("mod_remote_storage"));
      tenantController.postTenant(new TenantAttributes().purge(true));
      verify(jdbcTemplate).execute(String.format(DROP_SCHEMA_QUERY, getSchemaName()));
    }
  }

  private String getSchemaName() {
    return context.getFolioModuleMetadata().getDBSchemaName(context.getTenantId());
  }
}
