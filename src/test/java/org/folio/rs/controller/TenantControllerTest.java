package org.folio.rs.controller;

import static org.folio.rs.TestBase.getOkapiUrl;
import static org.mockito.Mockito.verify;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.folio.rs.domain.AsyncFolioExecutionContext;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionScopeExecutionContextManager;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource("classpath:application-test.yml")
@AutoConfigureEmbeddedDatabase(beanName = "dataSource")
public class TenantControllerTest {
  private final static String DROP_SCHEMA_QUERY = "DROP SCHEMA IF EXISTS %1$s CASCADE; DROP ROLE IF EXISTS %1$s";

  @Autowired
  private TenantController tenantController;

  @Autowired
  private FolioModuleMetadata moduleMetadata;

  @Autowired
  private FolioExecutionContext context;

  @SpyBean
  private JdbcTemplate jdbcTemplate;

  @Test
  void shouldDropDatabaseSchemaUponTenantDeletion() {
    FolioExecutionScopeExecutionContextManager.beginFolioExecutionContext(
      AsyncFolioExecutionContext.builder()
        .tenantId("test_tenant")
        .moduleMetadata(moduleMetadata)
        .okapiUrl(getOkapiUrl()).build());
    tenantController.postTenant(new TenantAttributes().moduleTo("mod_remote_storage"));
    tenantController.deleteTenant("test_tenant");

    verify(jdbcTemplate).execute(String.format(DROP_SCHEMA_QUERY, getSchemaName()));
  }

  private String getSchemaName() {
    return context.getFolioModuleMetadata().getDBSchemaName(context.getTenantId());
  }
}
