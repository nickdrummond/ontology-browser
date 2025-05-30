package org.ontbrowser.www.url;

import jakarta.servlet.http.HttpServletRequest;
import java.net.URI;

import static org.springframework.web.util.UriComponentsBuilder.fromUri;

public class GlobalPagingURIScheme extends PagingURIScheme {

    public GlobalPagingURIScheme(HttpServletRequest request) {
        super(request);
    }

    public URI updateForPage(String componentName, int start, int pageSize) {
        return fromUri(original)
                .replaceQueryParam("start", start)
                .replaceQueryParam("pageSize", pageSize)
                .build(true) // state that all existing params are already encoded (do not double-encode)
                .toUri();
    }
}
