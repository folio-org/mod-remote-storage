package org.folio.rs;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertThrows;

import org.junit.jupiter.api.Test;

class ModRemoteStorageApplicationTest {

  @Test
  void exceptionOnMissingSystemUserPassword() {
    var e = assertThrows(IllegalArgumentException.class, () -> ModRemoteStorageApplication.main(null));
    assertThat(e.getMessage(), containsString(ModRemoteStorageApplication.SYSTEM_USER_PASSWORD));
  }

}
