package org.folio.rs.client;

import feign.Client;
import feign.Request;
import feign.Request.Options;
import feign.Response;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

@Log4j2
public class EnrichHeadersClient extends Client.Default {

  @Autowired
  private FolioExecutionContext folioExecutionContext;

  public EnrichHeadersClient() {
    super(null, null);
  }

  @Override
  @SneakyThrows
  public Response execute(Request request, Options options) {

    FieldUtils.writeDeclaredField(request, "url",
      request.url().replace("http://", folioExecutionContext.getOkapiUrl() +"/"), true);

    return super.execute(request, options);
  }
}
