package org.folio.rs.client;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.folio.rs.domain.entity.FolioContext;
import org.springframework.beans.factory.annotation.Autowired;

import feign.Client;
import feign.Request;
import feign.Request.Options;
import feign.Response;
import lombok.SneakyThrows;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.folio.spring.integration.XOkapiHeaders.TOKEN;

@Log4j2
public class EnrichHeadersClient extends Client.Default {

  @Autowired
  private FolioContext folioContext;

  public EnrichHeadersClient() {
    super(null, null);
  }

  @Override
  @SneakyThrows
  public Response execute(Request request, Options options) {

    Map<String, Collection<String>> headers = new HashMap<>(request.headers());

    headers.put(TOKEN, Collections.singletonList(folioContext.getOkapiToken()));
    headers.put(TENANT, Collections.singletonList(folioContext.getTenant()));

    FieldUtils.writeDeclaredField(request, "headers", headers, true);
    FieldUtils.writeDeclaredField(request, "url", request.url().replace("http://", folioContext.getOkapiUrl()), true);

    log.info("Sending request to {} for tenant: {}", request.url(), request.headers().get(TENANT));
    Response response = super.execute(request, options);
    log.info("Response status: {}", response.status());

    return response;
  }
}
