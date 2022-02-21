package org.folio.rs.client.interceptor;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.folio.spring.integration.XOkapiHeaders.TOKEN;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import lombok.SneakyThrows;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collections;

@Component
public class FeignRequestInterceptor implements RequestInterceptor {

  @Autowired
  private FolioExecutionContext folioExecutionContext;

  @SneakyThrows
  @Override
  public void apply(RequestTemplate template) {
    if (!"/login".equals(template.path())) {
      template.header(TOKEN, Collections.singletonList(folioExecutionContext.getToken()));
    }
    template.header(TENANT, Collections.singletonList(folioExecutionContext.getTenantId()));
  }
}
