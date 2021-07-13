package org.folio.rs;

import org.folio.rs.controller.CheckInRetrieveTest;
import org.folio.rs.controller.ConfigurationsTest;
import org.folio.rs.controller.LocationMappingsTest;
import org.folio.rs.controller.ProviderControllerTest;
import org.folio.rs.controller.PubSubEventControllerTest;
import org.folio.rs.controller.ReturnItemTest;
import org.folio.rs.error.KafkaErrorHandlerTest;
import org.folio.rs.service.AccessionQueueServiceTest;
import org.folio.rs.service.CheckInItemServiceTest;
import org.folio.rs.service.KafkaServiceTest;
import org.folio.rs.service.ReturnRetrievalQueueServiceTest;
import org.folio.rs.service.ReturnRetrievalQueueServiceUnitTest;
import org.folio.rs.service.ReturnItemServiceTest;
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
  class AccessionQueueServiceTestNested extends AccessionQueueServiceTest {

  }

  @Nested
  class ReturnRetrievalQueueServiceTestNested extends ReturnRetrievalQueueServiceTest {

  }

  @Nested
  class CheckInRetrieveTestNested extends CheckInRetrieveTest {

  }

  @Nested
  class CheckInItemServiceTestNested extends CheckInItemServiceTest {

  }

  @Nested
  class ReturnItemServiceTestNested extends ReturnItemServiceTest {

  }

  @Nested
  class ReturnItemTestNested extends ReturnItemTest {

  }

  @Nested
  class ReturnRetrievalQueueServiceUnitTestNested extends ReturnRetrievalQueueServiceUnitTest {

  }

  @Nested
  class PubSubEventControllerTestNested extends PubSubEventControllerTest {

  }

  @Nested
  class ProviderControllerTestNested extends ProviderControllerTest {

  }

  @Nested
  class KafkaServiceTestNested extends KafkaServiceTest {

  }

  @Nested
  class KafkaErrorHandlerTestNested extends KafkaErrorHandlerTest {

  }
}
