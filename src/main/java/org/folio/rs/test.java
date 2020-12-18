package org.folio.rs;

import org.apache.commons.beanutils.BeanUtils;
import org.folio.rs.domain.entity.Configuration;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.UUID;

public class test {
  public static void main(String[] args) throws Exception {
    Configuration source = new Configuration();
    source.setId(UUID.randomUUID());
    source.setName("name1");
    source.setProviderName("provider1");
    source.setAccessionDelay(1);
    source.setAccessionTimeUnit("minutes");
    source.setUpdatedDate(Timestamp.valueOf(LocalDateTime.now().minusDays(1)));

    Configuration dest = new Configuration();
    dest.setId(UUID.randomUUID());
    dest.setName("name2");
    dest.setProviderName("provider2");
    dest.setAccessionDelay(2);
    dest.setAccessionTimeUnit("hours");
//    dest.setUpdatedDate(Timestamp.valueOf(LocalDateTime.now()));

    System.out.println(source);
    System.out.println(dest);

    BeanUtils.copyProperties(source, dest);

    System.out.println();
    System.out.println(source);
    System.out.println(dest);

  }
}
