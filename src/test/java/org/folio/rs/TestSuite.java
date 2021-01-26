package org.folio.rs;

import org.folio.rs.controller.ConfigurationsTest;
import org.folio.rs.controller.LocationMappingsTest;
import org.folio.rs.integration.KafkaIntegrationTest;
import org.folio.rs.service.SecurityManagerServiceTest;
import org.junit.jupiter.api.Nested;


public class TestSuite {

  @Nested
  class ConfigurationsTestNested extends ConfigurationsTest {

  }

  @Nested
  class LocationMappingsTestNested extends LocationMappingsTest {

  }

  @Nested
  class SecurityManagerServiceTestNested extends SecurityManagerServiceTest {

  }

  @Nested
  class KafkaIntegrationTestNested extends KafkaIntegrationTest {

  }
}
