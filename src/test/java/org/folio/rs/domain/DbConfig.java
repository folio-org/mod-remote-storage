package org.folio.rs.domain;

import de.flapdoodle.embed.process.runtime.Network;
import org.hibernate.cfg.AvailableSettings;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Profile;
import org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import ru.yandex.qatools.embed.postgresql.PostgresExecutable;
import ru.yandex.qatools.embed.postgresql.PostgresProcess;
import ru.yandex.qatools.embed.postgresql.PostgresStarter;
import ru.yandex.qatools.embed.postgresql.config.AbstractPostgresConfig;
import ru.yandex.qatools.embed.postgresql.config.PostgresConfig;
import ru.yandex.qatools.embed.postgresql.distribution.Version;

import javax.sql.DataSource;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

@Configuration
@EnableTransactionManagement
@Profile("TestDB")
public class DbConfig {
  private static final List<String> DEFAULT_ADDITIONAL_INIT_DB_PARAMS = Arrays
    .asList("--nosync", "--locale=en_US.UTF-8");

  @Bean
  @DependsOn("postgresProcess")
  public DataSource dataSource(PostgresConfig postgresConfig) {
    DriverManagerDataSource ds = new DriverManagerDataSource();
    ds.setDriverClassName("org.postgresql.Driver");
    ds.setUrl(String.format("jdbc:postgresql://%s:%s/%s", postgresConfig.net().host(), postgresConfig.net().port(),
      postgresConfig.storage().dbName()));
    ds.setUsername(postgresConfig.credentials().username());
    ds.setPassword(postgresConfig.credentials().password());
    return ds;
  }

  @Bean
  public LocalContainerEntityManagerFactoryBean entityManagerFactory(DataSource dataSource) {

    LocalContainerEntityManagerFactoryBean lcemfb = new LocalContainerEntityManagerFactoryBean();
    lcemfb.setDataSource(dataSource);
    lcemfb.setPackagesToScan("org.folio.rs.domain");
    HibernateJpaVendorAdapter va = new HibernateJpaVendorAdapter();
    lcemfb.setJpaVendorAdapter(va);
    lcemfb.setJpaProperties(getHibernateProperties());
    lcemfb.afterPropertiesSet();
    return lcemfb;
  }

  @Bean
  public JpaTransactionManager transactionManager(LocalContainerEntityManagerFactoryBean localContainerEntityManagerFactoryBean) {
    JpaTransactionManager transactionManager = new JpaTransactionManager();

    transactionManager.setEntityManagerFactory(localContainerEntityManagerFactoryBean.getObject());

    return transactionManager;
  }

  @Bean
  public PersistenceExceptionTranslationPostProcessor exceptionTranslation() {
    return new PersistenceExceptionTranslationPostProcessor();
  }

  private Properties getHibernateProperties() {
    Properties ps = new Properties();
    ps.put("hibernate.temp.use_jdbc_metadata_defaults", "false");
    ps.put("hibernate.dialect", "org.hibernate.dialect.PostgreSQL10Dialect");
    ps.put("hibernate.hbm2ddl.auto", "create");
    ps.put("hibernate.connection.characterEncoding", "UTF-8");
    ps.put("hibernate.connection.charSet", "UTF-8");

    ps.put(AvailableSettings.FORMAT_SQL, "true");
    ps.put(AvailableSettings.SHOW_SQL, "true");
    return ps;
  }

  @Bean
  public PostgresConfig postgresConfig() throws IOException {

    final PostgresConfig postgresConfig = new PostgresConfig(Version.V11_1,
      new AbstractPostgresConfig.Net("localhost", Network.getFreeServerPort()),
      new AbstractPostgresConfig.Storage("test"),
      new AbstractPostgresConfig.Timeout(),
      new AbstractPostgresConfig.Credentials("user", "pass")
    );

    postgresConfig.getAdditionalInitDbParams().addAll(DEFAULT_ADDITIONAL_INIT_DB_PARAMS);

    return postgresConfig;
  }

  @Bean(destroyMethod = "stop")
  public PostgresProcess postgresProcess(PostgresConfig config) throws IOException {
    PostgresStarter<PostgresExecutable, PostgresProcess> runtime = PostgresStarter.getDefaultInstance();
    PostgresExecutable exec = runtime.prepare(config);
    return exec.start();
  }
}
