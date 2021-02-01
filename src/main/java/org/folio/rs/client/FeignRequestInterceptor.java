package org.folio.rs.client;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.folio.spring.integration.XOkapiHeaders.TOKEN;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collections;

@Component
public class FeignRequestInterceptor implements RequestInterceptor {

  @Autowired
  private FolioExecutionContext folioContext;

  @Override
  public void apply(RequestTemplate template) {
    template.header(TOKEN, Collections.singletonList(folioContext.getToken()));
    template.header(TENANT, Collections.singletonList(folioContext.getTenantId()));
  }
}
