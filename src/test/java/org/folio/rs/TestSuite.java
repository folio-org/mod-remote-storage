package org.folio.rs;

import org.folio.rs.controller.CheckInRetrieveTest;
import org.folio.rs.controller.ConfigurationsTest;
import org.folio.rs.controller.LocationMappingsTest;
import org.folio.rs.controller.PubSubEventControllerTest;
import org.folio.rs.controller.ReturnItemTest;
import org.folio.rs.error.KafkaErrorHandlerTest;
import org.folio.rs.service.AccessionQueueServiceTest;
import org.folio.rs.service.CheckInItemServiceTest;
import org.folio.rs.service.RetrievalQueueServiceTest;
import org.folio.rs.service.RetrievalQueueServiceUnitTest;
import org.folio.rs.service.KafkaServiceTest;
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
  class RetrievalQueueServiceTestNested extends RetrievalQueueServiceTest {

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
  class RetrievalQueueServiceUnitTestNested extends RetrievalQueueServiceUnitTest {

  }

  @Nested
  class PubSubEventControllerTestNested extends PubSubEventControllerTest {

  }


  @Nested
  class KafkaServiceTestNested extends KafkaServiceTest {

  }

  @Nested
  class KafkaErrorHandlerTestNested extends KafkaErrorHandlerTest {

  }
}
