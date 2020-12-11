package org.folio.rs.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;

import org.folio.rs.domain.entity.RemoteStorageConfiguration;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.time.LocalDateTime;

public class RemoteStorageConfigurationEntityTest {
  @Test
  void shouldCopyUpdatedDateOrSetCurrentWhenNull() {
    RemoteStorageConfiguration configuration = new RemoteStorageConfiguration();
    configuration.setUpdatedDate(Timestamp.valueOf(LocalDateTime.now()));
    assertThat(new RemoteStorageConfiguration().copyForUpdate(configuration).getUpdatedDate(),
      equalTo(configuration.getUpdatedDate()));

    configuration.setUpdatedDate(null);
    assertThat(new RemoteStorageConfiguration().copyForUpdate(configuration).getUpdatedDate(), notNullValue());
  }
}
