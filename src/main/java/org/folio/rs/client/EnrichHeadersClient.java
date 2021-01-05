package org.folio.rs.client;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.reflect.FieldUtils;
import org.folio.rs.domain.entity.GlobalValue;
import org.springframework.beans.factory.annotation.Autowired;

import feign.Client;
import feign.Request;
import feign.Request.Options;
import feign.Response;
import lombok.SneakyThrows;

public class EnrichHeadersClient extends Client.Default {

  @Autowired
  private GlobalValue globalValue;

  public EnrichHeadersClient() {
    super(null, null);
  }

  @Override
  @SneakyThrows
  public Response execute(Request request, Options options) {
    Map<String, Collection<String>> headers = new HashMap<>(request.headers());
    FieldUtils.writeDeclaredField(request, "headers", headers, true);
    FieldUtils.writeDeclaredField(request, "url", request.url()
      .replace("http://", globalValue.getOkapiUrl()), true);
    return super.execute(request, options);
  }
}
