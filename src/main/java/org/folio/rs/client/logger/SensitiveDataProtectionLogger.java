package org.folio.rs.client.logger;

import feign.Logger;
import feign.Request;
import feign.Response;
import lombok.extern.slf4j.Slf4j;
import org.folio.spring.integration.XOkapiHeaders;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static feign.Util.valuesOrEmpty;

@Slf4j
public class SensitiveDataProtectionLogger extends Logger {

  @Override
  protected void logRequest(String configKey, Level logLevel, Request request) {
    super.logRequest(configKey, logLevel, getSensitiveDataProtectedRequest(request));
  }

  @Override
  protected Response logAndRebufferResponse(String configKey, Level logLevel, Response response, long elapsedTime)
    throws IOException {

    return super.logAndRebufferResponse(configKey, logLevel, getSensitiveDataProtectedResponse(response), elapsedTime)
      .toBuilder()
      .headers(response.headers())
      .build();
  }

  @Override
  protected void log(String configKey, String format, Object... args) {
    log.debug(format(configKey, format, args));
  }

  protected String format(String configKey, String format, Object... args) {
    return String.format(methodTag(configKey) + format, args);
  }

  private Request getSensitiveDataProtectedRequest(Request request) {
    return Request.create(request.httpMethod(), request.url(), maskOkapiTokenInHeaders(request.headers()), request.body(), Charset.forName("UTF-8"));
  }

  private Response getSensitiveDataProtectedResponse(Response response) {
    return response.toBuilder()
      .headers(maskOkapiTokenInHeaders(response.headers()))
      .build();
  }

  private Map<String, Collection<String>> maskOkapiTokenInHeaders(Map<String, Collection<String>> headers) {
    Map<String, Collection<String>> result = new HashMap<>();
    for (String field : headers.keySet()) {
      if(XOkapiHeaders.TOKEN.equals(field)) {
        List<String> tokens = new ArrayList<>();
        for (String value : valuesOrEmpty(headers, field)) {
          tokens.add("***" + value.substring(value.length() - 5));
        }
        result.put(field, tokens);
      } else {
        result.put(field, valuesOrEmpty(headers, field));
      }
    }
    return result;
  }
}
