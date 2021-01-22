package org.folio.rs.domain;

import org.hibernate.cfg.AvailableSettings;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.util.Properties;

@Configuration
@EnableTransactionManagement
@Profile("TestDB")
//TODO: I believe we should get rid of this manual DB configuration for tests
public class DbConfig {

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
    ps.put("hibernate.connection.characterEncoding", "UTF-8");
    ps.put("hibernate.physical_naming_strategy", "org.springframework.boot.orm.jpa.hibernate.SpringPhysicalNamingStrategy");
    ps.put("hibernate.implicit_naming_strategy", "org.springframework.boot.orm.jpa.hibernate.SpringImplicitNamingStrategy");

    ps.put(AvailableSettings.FORMAT_SQL, "true");
    ps.put(AvailableSettings.SHOW_SQL, "true");
    return ps;
  }
}
