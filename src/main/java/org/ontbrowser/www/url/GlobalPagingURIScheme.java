package org.ontbrowser.www.url;

import java.net.URI;

import static org.springframework.web.util.UriComponentsBuilder.fromUri;

public class GlobalPagingURIScheme extends PagingURIScheme {

    public GlobalPagingURIScheme(String queryString) {
        super(queryString);
    }

    public URI updateForPage(String componentName, int start, int pageSize) {
        return fromUri(original)
                .replaceQueryParam("start", start)
                .replaceQueryParam("pageSize", pageSize)
                .build(true) // state that all existing params are already encoded (do not double-encode)
                .toUri();
    }
}
